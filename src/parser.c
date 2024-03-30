#include <stdio.h>
#include <ctype.h>

#include "parser.h"
#include "log.h"
#include "arena.h"

typedef enum {
  TokenKindNone   = 1 << 0,
  TokenKindIntLit = 1 << 1,
  TokenKindStrLit = 1 << 2,
  TokenKindOp     = 1 << 3,
  TokenKindSemi   = 1 << 4,
  TokenKindIdent  = 1 << 5,
  TokenKindOParen = 1 << 6,
  TokenKindCParen = 1 << 7,
  TokenKindComma  = 1 << 8,
  TokenKindColon  = 1 << 9,
  TokenKindLet    = 1 << 10,
  TokenKindIf     = 1 << 11,
  TokenKindElse   = 1 << 12,
  TokenKindCount  = 13,
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

Str escape_str(Str str) {
  Str result = {
    .ptr = aalloc(str.len),
    .len = 0,
  };

  bool escaped = false;
  for (i32 i = 0; i < str.len; ++i) {
    if (escaped) {
      switch (str.ptr[i]) {
      case 'n': result.ptr[result.len++] = '\n'; break;
      case 'r': result.ptr[result.len++] = '\r'; break;
      case 't': result.ptr[result.len++] = '\t'; break;
      case '\\': result.ptr[result.len++] = '\\'; break;
      case '"': result.ptr[result.len++] = '"'; break;
      default:
        ERROR("Unknown escape character: %c", result.ptr[result.len]);
        exit(1);
      }
    } else if (str.ptr[i] != '\\') {
      result.ptr[result.len++] = str.ptr[i];
    }

    escaped = !escaped && str.ptr[i] == '\\';
  }

  return result;
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
          .row = -1,
          .col = -1,
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

  _Static_assert (TokenKindCount == 13, "All token kinds should be handled here.");
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
  } else if (isalpha(*start) || *start == '_') {
    kind = TokenKindIdent;
    while (!parser_eof(parser) &&
           (isalnum(*parser->current) ||
            *parser->current == '_'))
      parser->current++;

    struct Keyword {
      Str keyword;
      i32 token_kind;
    };

    _Static_assert (TokenKindCount == 13, "Maybe new keyword was added? Then update this place.");
    static struct Keyword keywords[] = {
      { STR("let", 3), TokenKindLet },
      { STR("if", 2), TokenKindIf },
      { STR("else", 4), TokenKindElse },
    };

    for (i32 i = 0; i < (i32) ARRAY_LEN(keywords); ++i) {
      if (str_eq(keywords[i].keyword, STR(start, parser->current - start))) {
        kind = keywords[i].token_kind;
        break;
      }
    }
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

  Str str = {
    .ptr = start,
    .len = parser->current - start,
  };
  if (kind == TokenKindStrLit)
    str = escape_str(str);

  return parser->last_token = (Token) {
    .kind = kind,
    .str = str,
    .row = parser->row + 1,
    .col = start - parser->bol + 1,
  };
}

static Token parser_peek_token(Parser *parser) {
  Parser parser_copy = *parser;
  Token token = parser_next_token(&parser_copy);

  return token;
}

static void print_token_kind(TokenKind token_kind) {
  char *prev_str = NULL;
  bool printed = false;

  _Static_assert (TokenKindCount == 13, "All token kinds should be handled here.");
  static char *token_kind_names[TokenKindCount] = {
    "end of file", "integer", "string", "operator", "semicolon",
    "identifier", "left paren", "right paren", "comma", "colon",
    "let", "if", "else",
  };

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
    if (token.row == -1 || token.col == -1)
      token = parser_next_token(parser);

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
  Args args = {0};
  bool func = false;

  Token name = parser_expect_token(parser, TokenKindIdent);
  Token token = parser_expect_token(parser, TokenKindOParen | TokenKindOp);

  if (token.kind == TokenKindOParen) {
    func = true;

    while (parser_peek_token(parser).kind != TokenKindCParen) {
      Token arg_name = parser_expect_token(parser, TokenKindIdent);
      DA_APPEND(args, arg_name.str);

      if (parser_peek_token(parser).kind != TokenKindCParen)
        parser_expect_token(parser, TokenKindComma);
    }

    parser_next_token(parser);
    token = parser_expect_token(parser, TokenKindOp);
  }

  if (!str_eq(token.str, STR("=", 1))) {
    PERROR("%s:%d:%d: ", "Unexpected ",
           parser->file_path, token.row, token.col);
    print_token_kind(token.kind);
    fputs(", expected `=` in `let` expression\n", stderr);
    exit(1);
  }

  Expr value = parser_parse_expr(parser, 0);

  Expr expr;

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
  if (token.kind == TokenKindElse) {
    parser_next_token(parser);
    token = parser_expect_token(parser, TokenKindColon | TokenKindIf);
    if (token.kind == TokenKindIf) {
      eef.as.eef->elze = parser_parse_if(parser);
    } else {
      eef.as.eef->elze = parser_parse_expr(parser, 0);
    }
    eef.as.eef->has_else = true;
  }

  return eef;
}

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token = parser_expect_token(parser,
                                    TokenKindIntLit | TokenKindIdent |
                                    TokenKindOParen | TokenKindStrLit |
                                    TokenKindLet | TokenKindIf);

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
    if (lhs.as.block->len == 1) {
      Expr *items = lhs.as.block->items;
      lhs = lhs.as.block->items[0];
      free(items);
    }
  } else if (token.kind == TokenKindLet) {
    lhs = parser_parse_let(parser);
  } else if (token.kind == TokenKindIf) {
    lhs = parser_parse_if(parser);
  }

  if (parser_peek_token(parser).kind == TokenKindOParen) {
    parser_next_token(parser);
    Expr func = lhs;
    Expr args = parser_parse_block(parser, TokenKindComma, TokenKindCParen);

    lhs.kind = ExprKindCall;
    lhs.as.call = aalloc(sizeof(ExprCall));
    lhs.as.call->func = func;
    lhs.as.call->args = args.as.block;
  }

  return lhs;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence) {
  Expr lhs = parser_parse_lhs(parser);
  ExprCall *expr = NULL;

  for (;;) {
    Token token = parser_peek_token(parser);
    i32 precedence = op_precedence(token.str);
    if (token.kind != TokenKindOp || precedence <= min_precedence)
      break;
    parser_next_token(parser);

    Expr rhs = parser_parse_expr(parser, precedence);

    expr = aalloc(sizeof(ExprCall));
    expr->func.kind = ExprKindIdent;
    expr->func.as.ident = aalloc(sizeof(ExprIdent));
    expr->func.as.ident->ident = token.str;
    expr->args = aalloc(sizeof(ExprBlock));
    *expr->args = (ExprBlock) {
      .items = aalloc(sizeof(Expr) * 2),
      .len = 2,
      .cap = 2,
    };
    expr->args->items[0] = lhs;
    expr->args->items[1] = rhs;

    lhs.kind = ExprKindCall;
    lhs.as.call = expr;
  }

  return lhs;
}

static Expr parser_parse_block(Parser *parser, TokenKind sep, TokenKind end_with) {
  ExprBlock *block;
  block = aalloc(sizeof(ExprBlock));
  *block = (ExprBlock) {
    .items = NULL,
    .len = 0,
    .cap = 0,
  };

  while (parser_peek_token(parser).kind != end_with) {
    Expr expr = parser_parse_expr(parser, 0);
    DA_APPEND(*block, expr);

    if (parser_peek_token(parser).kind != end_with)
      parser_expect_token(parser, sep);
  }

  parser_next_token(parser);

  return (Expr) {
    .kind = ExprKindBlock,
    .as = {
      .block = block,
    },
  };
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
