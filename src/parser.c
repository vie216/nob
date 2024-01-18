#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "parser.h"
#include "log.h"

typedef enum {
  TokenKindNone   = 0,
  TokenKindIntLit = 1 << 0,
  TokenKindOp     = 1 << 1,
  TokenKindSep    = 1 << 2,
  TokenKindIdent  = 1 << 3,
  TokenKindOParen = 1 << 4,
  TokenKindCParen = 1 << 5,
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
                    || ch == '#')
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
    { STR(",", 1), 0 },
    { STR("=", 1), 1 },
    { STR("||", 2), 2 },
    { STR("&&", 2), 3 },
    { STR("|", 1), 5 },
    { STR("^", 1), 6 },
    { STR("&", 1), 7 },
    { STR("+", 1), 8 }, { STR("-", 1), 8 },
    { STR("*", 1), 9 }, { STR("/", 1), 9 }, { STR("%", 1), 9 },
  };

  for (i32 i = 0; i < (int) ARRAY_LEN(precedences); ++i)
    if (str_eq(op, precedences[i].op))
      return precedences[i].precedence;

  return 4;
}

static bool parser_eof(Parser *parser) {
  return parser->current >= parser->source.ptr + parser->source.len;
}

static Token parser_next_token(Parser *parser) {
  TokenKind kind;
  char *start;

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

  start = parser->current++;

  kind = TokenKindNone;

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
  }

  if (kind == TokenKindNone) {
    PERROR("%s:%d:%d: ", "Unknown token\n",
           parser->file_path, parser->row + 1,
           (int) (start - parser->bol + 1));
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
  Parser parser_backup;
  Token token;

  parser_backup = *parser;
  token = parser_next_token(parser);
  *parser = parser_backup;

  return token;
}

static Token parser_expect_token(Parser *parser, TokenKind expected_kind) {
  Token token;

  token = parser_next_token(parser);
  if (!(token.kind & expected_kind)) {
    PERROR("%s:%d:%d: ", "Unexpected token\n", parser->file_path, token.row, token.col);
    exit(1);
  }

  return token;
}

static Expr parser_parse_block(Parser *parser, TokenKind end_with);

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token;

  token = parser_expect_token(parser, TokenKindIntLit | TokenKindIdent | TokenKindOParen);

  if (token.kind == TokenKindIntLit) {
    lhs.int_lit = malloc(sizeof(ExprIntLit));
    lhs.int_lit->kind = ExprKindIntLit;
    lhs.int_lit->lit = token.str;
  } else if (token.kind == TokenKindIdent) {
    lhs.ident = malloc(sizeof(ExprIdent));
    lhs.ident->kind = ExprKindIdent;
    lhs.ident->ident = token.str;
  } else if (token.kind == TokenKindOParen) {
    lhs = parser_parse_block(parser, TokenKindCParen);
  }

  return lhs;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence) {
  Expr lhs;
  ExprBinOp *bin_op = NULL;
  Token token;
  i32 precedence;

  lhs = parser_parse_lhs(parser);

  for (;;) {
    token = parser_peek_token(parser);
    precedence = op_precedence(token.str);
    if (token.kind != TokenKindOp || precedence <= min_precedence)
      break;
    parser_next_token(parser);

    if (!bin_op) {
      bin_op = malloc(sizeof(ExprBinOp));
      bin_op->kind = ExprKindBinOp;
    }

    bin_op->op = token.str;
    bin_op->lhs = lhs;
    bin_op->rhs = parser_parse_expr(parser, precedence);
    lhs.bin_op = bin_op;
  }

  return lhs;
}

static Expr parser_parse_block(Parser *parser, TokenKind end_with) {
  Expr block, expr;

  block.block = malloc(sizeof(ExprBlock));
  block.block->kind = ExprKindBlock;
  block.block->len = 0;
  block.block->cap = 0;

  while (parser_peek_token(parser).kind != end_with) {
    expr = parser_parse_expr(parser, 0);
    DA_APPEND(*block.block, expr);

    if (parser_peek_token(parser).kind != end_with)
      parser_expect_token(parser, TokenKindSep);
  }

  parser_next_token(parser);

  return block;
}

Expr parse_program(Str source, char *file_path) {
  Parser parser;

  parser = (Parser) {
    .source = source,
    .file_path = file_path,
    .current = source.ptr,
    .bol = source.ptr,
    .row = 0,
    .last_token = (Token) { .kind = TokenKindNone },
  };

  return parser_parse_block(&parser, TokenKindNone);
}
