#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "parser.h"
#include "log.h"

typedef enum {
  TokenKindNone   = 0,
  TokenKindIntLit = 1 << 0,
  TokenKindStrLit = 1 << 1,
  TokenKindOp     = 1 << 2,
  TokenKindSep    = 1 << 3,
  TokenKindIdent  = 1 << 4,
  TokenKindOParen = 1 << 5,
  TokenKindCParen = 1 << 6,
  TokenKindArgSep = 1 << 7,
} TokenKind;

typedef struct {
  TokenKind kind;
  Str       str;
  i32       row, col;
} Token;

typedef struct {
  Str    source;
  char  *file_path;
  char  *current, *bol;
  i32    row;
  Token  last_token;
} Parser;

#define IS_OP(ch) !(isalnum(ch) || isspace(ch)  \
                    || ch == '"' || ch == '\''  \
                    || ch == '(' || ch == ')'   \
                    || ch == '{' || ch == '}'   \
                    || ch == '[' || ch == ']'   \
                    || ch == ';' || ch == ':'   \
                    || ch == '#' || ch == ',')
#define IS_EXPR_END(token) ((token).kind == TokenKindIntLit     \
                            || (token).kind == TokenKindIdent   \
                            || (token).kind == TokenKindCParen)
#define IS_IDENT(ch) (isalpha(ch) || ch == '_')

static i32 op_precedence(Str op) {
  struct OpPrecedence {
    Str op;
    i32 precedence;
  };

  static struct OpPrecedence precedences[] = {
    { STR("=", 1), 0 },
    { STR("||", 2), 1 },
    { STR("&&", 2), 2 },
    { STR("|", 1), 4 },
    { STR("^", 1), 5 },
    { STR("&", 1), 6 },
    { STR("+", 1), 7 }, { STR("-", 1), 7 },
    { STR("*", 1), 8 }, { STR("/", 1), 8 }, { STR("%", 1), 8 },
  };

  for (i32 i = 0; i < (int) ARRAY_LEN(precedences); ++i)
    if (str_eq(op, precedences[i].op))
      return precedences[i].precedence;

  return 3;
}

static bool parser_eof(Parser *parser) {
  return parser->current >= parser->source.ptr + parser->source.len;
}

static Token parser_next_token(Parser *parser) {
  while (!parser_eof(parser) && (isspace(*parser->current) || *parser->current == '#')) {
    if (*parser->current == '#') {
      while (!parser_eof(parser) && *parser->current != '\n')
        parser->current++;
    }

    if (*parser->current == '\n') {
      parser->bol = parser->current + 1;
      parser->row++;

      if (IS_EXPR_END(parser->last_token))
        return parser->last_token = (Token) {
          .kind = TokenKindSep,
          .str = STR(";", 1),
          .row = parser->row + 1,
          .col = parser->current - parser->bol + 1,
        };
    }

    parser->current++;
  }

  if (parser_eof(parser))
    return (Token) { .kind = TokenKindNone };

  TokenKind kind = TokenKindNone;
  char *start = parser->current++;

  if (isdigit(*start)) {
    kind = TokenKindIntLit;
    while (!parser_eof(parser) && isdigit(*parser->current))
      parser->current++;
  } else if (IS_OP(*start)) {
    kind = TokenKindOp;
    while (!parser_eof(parser) && IS_OP(*parser->current))
      parser->current++;
  } else if (*start == ';') {
    kind = TokenKindSep;
  } else if (IS_IDENT(*start)) {
    kind = TokenKindIdent;
    while (!parser_eof(parser) && IS_IDENT(*parser->current))
      parser->current++;
  } else if (*start == '(') {
    kind = TokenKindOParen;
  } else if (*start == ')') {
    kind = TokenKindCParen;
  } else if (*start == ',') {
    kind = TokenKindArgSep;
  } else if (*start == '"') {
    kind = TokenKindStrLit;
    bool escaped;
    while (!parser_eof(parser) && (*parser->current != '"' || escaped)) {
      escaped = *parser->current == '\\';
      parser->current++;
    }

    if (parser_eof(parser)) {
      PERROR("%s:%d:%d: ", "Unclosed string literal\n",
             parser->file_path, parser->row + 1,
             (i32) (start - parser->bol + 1));
      exit(1);
    }

    parser->current++;
  }

  if (kind == TokenKindNone) {
    PERROR("%s:%d:%d: ", "Unknown token\n",
           parser->file_path, parser->row + 1,
           (i32) (start - parser->bol + 1));
    exit(1);
  }

  return parser->last_token = (Token) {
    .kind = kind,
    .str = {
      .ptr = start,
      .len = (i32) (parser->current - start),
    },
    .row = parser->row + 1,
    .col = start - parser->bol + 1,
  };
}

static Token parser_peek_token(Parser *parser) {
  Parser parser_copy = *parser;
  Token token = parser_next_token(&parser_copy);

  return token;
}

static Token parser_expect_token(Parser *parser, TokenKind expected_kind) {
  Token token = parser_next_token(parser);
  if (!(token.kind & expected_kind)) {
    PERROR("%s:%d:%d: ", "Unexpected token\n", parser->file_path, token.row, token.col);
    exit(1);
  }

  return token;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence);
static Expr parser_parse_block(Parser *parser, TokenKind sep, TokenKind end_with);

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token = parser_expect_token(parser,
                              TokenKindIntLit | TokenKindIdent |
                              TokenKindOParen | TokenKindStrLit);

  if (token.kind == TokenKindIntLit) {
    lhs.kind = ExprKindIntLit;
    lhs.as.int_lit = malloc(sizeof(ExprIntLit));
    lhs.as.int_lit->lit = token.str;
  } else if (token.kind == TokenKindIdent) {
    if (parser_peek_token(parser).kind == TokenKindOParen) {
      parser_next_token(parser);
      Expr args = parser_parse_block(parser, TokenKindArgSep, TokenKindCParen);

      lhs.kind = ExprKindCall;
      lhs.as.call = malloc(sizeof(ExprCall));
      lhs.as.call->name = token.str;
      lhs.as.call->args = *args.as.block;
      free(args.as.block);
    } else if (str_eq(token.str, STR("let", 3))) {
      Token name = parser_expect_token(parser, TokenKindIdent);
      Token op = parser_expect_token(parser, TokenKindOp);
      if (!str_eq(op.str, STR("=", 1))) {
        PERROR("%s:%d:%d: ", "Expected `=` in `let` expression\n",
               parser->file_path, op.row, op.col);
        exit(1);
      }
      Expr value = parser_parse_expr(parser, 0);

      lhs.kind = ExprKindVar;
      lhs.as.var = malloc(sizeof(ExprVar));
      lhs.as.var->name = name.str;
      lhs.as.var->value = value;
    } else {
      lhs.kind = ExprKindIdent;
      lhs.as.ident = malloc(sizeof(ExprIdent));
      lhs.as.ident->ident = token.str;
    }
  } else if (token.kind == TokenKindOParen) {
    lhs = parser_parse_block(parser, TokenKindSep, TokenKindCParen);
  } else if (token.kind == TokenKindStrLit) {
    lhs.kind = ExprKindStrLit;
    lhs.as.str_lit = malloc(sizeof(ExprStrLit));
    lhs.as.int_lit->lit = token.str;
  }

  return lhs;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence) {
  Expr lhs = parser_parse_lhs(parser);
  ExprBinOp *expr = NULL;

  for (;;) {
    Token token = parser_peek_token(parser);
    i32 precedence = op_precedence(token.str);
    if (token.kind != TokenKindOp || precedence <= min_precedence)
      break;
    parser_next_token(parser);

    expr = malloc(sizeof(ExprBinOp));
    expr->op = token.str;
    expr->lhs = lhs;
    expr->rhs = parser_parse_expr(parser, precedence);
    lhs.kind = ExprKindBinOp;
    lhs.as.bin_op = expr;
  }

  return lhs;
}

static Expr parser_parse_block(Parser *parser, TokenKind sep, TokenKind end_with) {
  Expr block;
  block.kind = ExprKindBlock;
  block.as.block = malloc(sizeof(ExprBlock));
  block.as.block->len = 0;
  block.as.block->cap = 0;

  while (parser_peek_token(parser).kind != end_with) {
    Expr expr = parser_parse_expr(parser, 0);
    DA_APPEND(*block.as.block, expr);

    if (parser_peek_token(parser).kind != end_with)
      parser_expect_token(parser, sep);
  }

  parser_next_token(parser);

  return block;
}

Expr parse_program(Str source, char *file_path) {
  Parser parser = (Parser) {
    .source = source,
    .file_path = file_path,
    .current = source.ptr,
    .bol = source.ptr,
    .row = 0,
    .last_token = (Token) { .kind = TokenKindNone },
  };

  return parser_parse_block(&parser, TokenKindSep, TokenKindNone);
}
