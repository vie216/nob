#include <stdio.h>
#include <ctype.h>

#include "parser.h"
#include "log.h"
#include "arena.h"

typedef enum {
  TokenKindNone   =  1 << 0,
  TokenKindIntLit =  1 << 1,
  TokenKindStrLit =  1 << 2,
  TokenKindOp     =  1 << 3,
  TokenKindSemi   =  1 << 4,
  TokenKindIdent  =  1 << 5,
  TokenKindOParen =  1 << 6,
  TokenKindCParen =  1 << 7,
  TokenKindComma  =  1 << 8,
  TokenKindColon  =  1 << 9,
  TokenKindKeyword = 1 << 10,
  TokenKindCount  = 11,
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

#define IS_OP(ch) !(isalnum(ch) || isspace(ch) \
                    || ch == '"' || ch == '\'' \
                    || ch == '(' || ch == ')'  \
                    || ch == '{' || ch == '}'  \
                    || ch == '[' || ch == ']'  \
                    || ch == ';' || ch == ':'  \
                    || ch == '#' || ch == ',')
#define IS_EXPR_END(token) ((token).kind == TokenKindIntLit     \
                            || (token).kind == TokenKindStrLit  \
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

static bool is_keyword(Str str) {
  static Str keywords[] = {
    STR("let", 3),
    STR("if", 2),
    STR("else", 4),
  };

  for (i32 i = 0; i < (i32) ARRAY_LEN(keywords); ++i)
    if (str_eq(keywords[i], str))
      return true;
  return false;
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

      if (IS_EXPR_END(parser->last_token)) {
        parser->current++;
        return parser->last_token = (Token) {
          .kind = TokenKindSemi,
          .str = STR(";", 1),
          .row = parser->row + 1,
          .col = parser->current - parser->bol + 1,
        };
      }
    }

    parser->current++;
  }

  if (parser_eof(parser))
    return (Token) {
      .kind = TokenKindNone,
      .row = parser->row + 1,
      .col = parser->current - parser->bol + 1,
    };

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
    kind = TokenKindSemi;
  } else if (IS_IDENT(*start)) {
    kind = TokenKindIdent;
    while (!parser_eof(parser) && IS_IDENT(*parser->current))
      parser->current++;
    bool keyword = is_keyword(STR(start, parser->current - start));
    kind = keyword ? TokenKindKeyword : TokenKindIdent;
  } else if (*start == '(') {
    kind = TokenKindOParen;
  } else if (*start == ')') {
    kind = TokenKindCParen;
  } else if (*start == ',') {
    kind = TokenKindComma;
  } else if (*start == ':') {
    kind = TokenKindColon;
  } else if (*start == '"') {
    kind = TokenKindStrLit;
    bool escaped = false;
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

static char *token_kind_names[TokenKindCount] = {
  "end of file", "integer", "string", "operator", "semicolon",
  "identifier", "left paren", "right paren", "comma", "colon",
  "keyword",
};

static void print_token_kind(TokenKind token_kind) {
  char *prev_str = NULL;
  bool printed = false;

  for (i32 i = 0; i < TokenKindCount; ++i) {
    if (token_kind & (1 << i)) {
      if (prev_str) {
        if (printed)
          fputs(", ", stderr);
        fputs(prev_str, stderr);

        printed = true;
      }

      prev_str = token_kind_names[i];
    }
  }

  if (prev_str) {
    if (printed)
      fputs(" or ", stderr);
    fputs(prev_str, stderr);
  }
}

static Token parser_expect_token(Parser *parser, TokenKind expected_kind) {
  Token token = parser_next_token(parser);
  if (!(token.kind & expected_kind)) {
    PERROR("%s:%d:%d: ", "Unexpected ",
           parser->file_path, token.row, token.col);
    print_token_kind(token.kind);
    fputs(", expected ", stderr);
    print_token_kind(expected_kind);
    putc('\n', stderr);
    exit(1);
  }

  return token;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence);
static Expr parser_parse_block(Parser *parser, TokenKind sep, TokenKind end_with);

static Expr parser_parse_let(Parser *parser) {
  Expr expr;
  Args args = {0};
  bool func;

  Token name = parser_expect_token(parser, TokenKindIdent);

  if (parser_peek_token(parser).kind == TokenKindOParen) {
    func = true;

    parser_next_token(parser);

    while (parser_peek_token(parser).kind != TokenKindCParen) {
      Token arg_name = parser_expect_token(parser, TokenKindIdent);
      DA_APPEND(args, arg_name.str);

      if (parser_peek_token(parser).kind != TokenKindCParen)
        parser_expect_token(parser, TokenKindComma);
    }

    parser_next_token(parser);
  }

  Token op = parser_expect_token(parser, TokenKindOp);
  if (!str_eq(op.str, STR("=", 1))) {
    PERROR("%s:%d:%d: ", "Expected `=` in `let` expression\n",
           parser->file_path, op.row, op.col);
    exit(1);
  }
  Expr value = parser_parse_expr(parser, 0);

  if (func) {
    expr.kind = ExprKindFunc;
    expr.as.func = aalloc(sizeof(ExprFunc));
    expr.as.func->name = name.str;
    expr.as.func->args = args;
    expr.as.func->body = value;
  } else {
    expr.kind = ExprKindVar;
    expr.as.var = aalloc(sizeof(ExprVar));
    expr.as.var->name = name.str;
    expr.as.var->value = value;
  }

  return expr;
}

static Expr parser_parse_if(Parser *parser) {
  Expr eef;

  eef.kind = ExprKindIf;
  eef.as.eef = aalloc(sizeof(ExprIf));
  eef.as.eef->cond = parser_parse_expr(parser, 0);
  parser_expect_token(parser, TokenKindColon);
  eef.as.eef->body = parser_parse_expr(parser, 0);
  eef.as.eef->has_else = false;

  parser_expect_token(parser, TokenKindSemi);

  Token token = parser_peek_token(parser);
  if (token.kind == TokenKindKeyword && str_eq(token.str, STR("else", 4))) {
    parser_next_token(parser);
    parser_expect_token(parser, TokenKindColon);
    eef.as.eef->elze = parser_parse_expr(parser, 0);
    eef.as.eef->has_else = true;
  }

  return eef;
}

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token = parser_expect_token(parser,
                              TokenKindIntLit | TokenKindIdent |
                              TokenKindOParen | TokenKindStrLit |
                              TokenKindKeyword);

  if (token.kind == TokenKindIntLit) {
    lhs.kind = ExprKindLit;
    lhs.as.lit = aalloc(sizeof(ExprLit));
    lhs.as.lit->kind = LitKindInt;
    lhs.as.lit->lit = token.str;
  } else if (token.kind == TokenKindStrLit) {
    lhs.kind = ExprKindLit;
    lhs.as.lit = aalloc(sizeof(ExprLit));
    lhs.as.lit->kind = LitKindStr;
    lhs.as.lit->lit = token.str;
  } else if (token.kind == TokenKindIdent) {
    lhs.kind = ExprKindIdent;
    lhs.as.ident = aalloc(sizeof(ExprIdent));
    lhs.as.ident->ident = token.str;
  } else if (token.kind == TokenKindOParen) {
    lhs = parser_parse_block(parser, TokenKindSemi, TokenKindCParen);
  } else if (token.kind == TokenKindKeyword) {
    if (str_eq(token.str, STR("let", 3))) {
      lhs = parser_parse_let(parser);
    } else if (str_eq(token.str, STR("if", 2))) {
      lhs = parser_parse_if(parser);
    }
  }

  if (parser_peek_token(parser).kind == TokenKindOParen) {
    parser_next_token(parser);
    Expr func = lhs;
    Expr args = parser_parse_block(parser, TokenKindComma , TokenKindCParen);

    lhs.kind = ExprKindCall;
    lhs.as.call = aalloc(sizeof(ExprCall));
    lhs.as.call->func = func;
    lhs.as.call->args = args.as.block;
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

    expr = aalloc(sizeof(ExprBinOp));
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
  block.as.block = aalloc(sizeof(ExprBlock));
  block.as.block->len = 0;
  block.as.block->cap = 0;

  while (parser_peek_token(parser).kind != end_with) {
    Expr expr = parser_parse_expr(parser, 0);
    DA_APPEND(*block.as.block, expr);

    if (parser_peek_token(parser).kind != end_with)
      parser_expect_token(parser, sep);
  }

  parser_next_token(parser);

  if (block.as.block->len == 1) {
    Expr expr = block.as.block->items[0];
    free(block.as.block->items);
    return expr;
  }

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

  return parser_parse_block(&parser, TokenKindSemi, TokenKindNone);
}
