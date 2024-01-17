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

#define IS_EXPR_END(token) ((token).kind == TokenKindIntLit)

static i32 op_precedence(Str op) {
  struct OpPrecedence {
    Str op;
    i32 precedence;
  };

  static struct OpPrecedence precedences[] = {
    { STR(",", 1), 0 },
    { STR("=", 1), 1 },
    { STR("&&", 2), 2 }, { STR("||", 2), 2 },
    { STR("&", 1), 4 }, { STR("|", 1), 4 }, { STR("^", 1), 4 },
    { STR("+", 1), 5 }, { STR("-", 1), 5 },
    { STR("*", 1), 6 }, { STR("/", 1), 6 }, { STR("%", 1), 6 },
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
  TokenKind kind;
  char *start;

  while (!parser_eof(parser) && isspace(*parser->current)) {
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
  }

  if (kind == TokenKindNone) {
    ERROR("Unknown token\n");
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

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token;

  token = parser_expect_token(parser, TokenKindIntLit);

  if (token.kind == TokenKindIntLit) {
    lhs.int_lit = malloc(sizeof(ExprIntLit));
    lhs.int_lit->kind = ExprKindIntLit;
    lhs.int_lit->lit = token.str;
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
  Expr block;
  i32  block_cap;

  block.block = malloc(sizeof(ExprBlock));
  block.block->kind = ExprKindBlock;
  block.block->exprs = malloc(sizeof(Expr));
  block.block->len = 0;
  block_cap = 1;

  while (parser_peek_token(parser).kind != end_with) {
    if (block_cap <= block.block->len)
      block.block->exprs = realloc(block.block->exprs, (block_cap *= 2) * sizeof(Expr));
    block.block->exprs[block.block->len++] = parser_parse_expr(parser, 0);

    if (parser_peek_token(parser).kind != end_with)
      parser_expect_token(parser, TokenKindSep);
  }

  parser_peek_token(parser);

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
