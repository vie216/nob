#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "parser.h"
#include "log.h"
#include "arena.h"
#include "io.h"

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
  TokenKindElif   = 1 << 13,
  TokenKindWhile  = 1 << 14,
  TokenKindRet    = 1 << 15,
  TokenKindAsm    = 1 << 16,
  TokenKindOSqr   = 1 << 17,
  TokenKindCSqr   = 1 << 18,
  TokenKindUse    = 1 << 19,
  TokenKindCount  = 20,
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

#define IS_OP(ch) !(isalnum(ch) || isspace(ch) ||  \
                    ch == '"' || ch == '\'' ||     \
                    ch == '(' || ch == ')'  ||     \
                    ch == '{' || ch == '}'  ||     \
                    ch == '[' || ch == ']'  ||     \
                    ch == ';' || ch == ':'  ||     \
                    ch == '#' || ch == ',')
#define IS_EXPR_END(token) ((token).kind == TokenKindIntLit ||  \
                            (token).kind == TokenKindStrLit ||  \
                            (token).kind == TokenKindIdent  ||  \
                            (token).kind == TokenKindCParen ||  \
                            (token).kind == TokenKindCSqr)

static i32 op_precedence(Str op) {
  struct OpPrecedence {
    Str op;
    i32 precedence;
  };

  static struct OpPrecedence precedences[] = {
    { STR("=", 1), 1 },
    { STR_LIT("=="), 2 },
    { STR_LIT("!="), 2 },
    { STR_LIT(">"), 2 },
    { STR_LIT("<"), 2 },
    { STR_LIT(">="), 2 },
    { STR_LIT("<="), 2 },
    { STR_LIT("|"), 3 },
    { STR_LIT("^"), 4 },
    { STR_LIT("&"), 5 },
    { STR_LIT("+"), 6 },
    { STR_LIT("-"), 6 },
    { STR_LIT("*"), 7 },
    { STR_LIT("/"), 7 },
    { STR_LIT("%"), 7 },
  };

  for (i32 i = 0; i < (int) ARRAY_LEN(precedences); ++i)
    if (str_eq(op, precedences[i].op))
      return precedences[i].precedence;

  return 4;
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
          .str = STR_LIT(";"),
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

  _Static_assert (TokenKindCount == 20, "All token kinds should be handled here.");
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

    _Static_assert (TokenKindCount == 20, "Maybe new keyword was added? Then update this place.");
    static struct Keyword keywords[] = {
      { STR_LIT("let"),   TokenKindLet },
      { STR_LIT("if"),    TokenKindIf },
      { STR_LIT("else"),  TokenKindElse },
      { STR_LIT("elif"),  TokenKindElif },
      { STR_LIT("while"), TokenKindWhile },
      { STR_LIT("ret"),   TokenKindRet },
      { STR_LIT("asm"),   TokenKindAsm },
      { STR_LIT("use"),   TokenKindUse },
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
  } else if (*start == '[') {
    kind = TokenKindOSqr;
  } else if (*start == ']') {
    kind = TokenKindCSqr;
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

  if (kind == TokenKindStrLit) {
    str.ptr += 1;
    str.len -= 2;
    str = escape_str(str);
  }

  return parser->last_token = (Token) {
    .kind = kind,
    .str = str,
    .row = parser->row + 1,
    .col = start - parser->bol + 1,
  };
}

static Token parser_peek_token(Parser parser, i32 n) {
  Token token = {0};

  for (i32 i = 0; i < n; ++i)
    token = parser_next_token(&parser);

  return token;
}

static void print_token_kind(TokenKind token_kind) {
  char *prev_str = NULL;
  bool printed = false;

  _Static_assert (TokenKindCount == 20, "All token kinds should be handled here.");
  static char *token_kind_names[TokenKindCount] = {
    "end of file", "integer", "string", "operator", "semicolon",
    "identifier", "left paren", "right paren", "comma", "colon",
    "let", "if", "else", "elif", "while", "ret", "asm", "use",
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

static TypeExpr parser_parse_type_expr(Parser *parser) {
  Token token = parser_expect_token(parser, TokenKindOParen | TokenKindIdent | TokenKindOp);

  if (token.kind == TokenKindOParen) {
    parser_expect_token(parser, TokenKindCParen);
    return (TypeExpr) { TypeExprKindUnit };
  } else if (token.kind == TokenKindIdent) {
    TypeExprIdent *ident = aalloc(sizeof(TypeExprIdent));
    ident->ident = token.str;
    return (TypeExpr) {
      .kind = TypeExprKindIdent,
      .as = { .ident = ident },
    };
  } else if (token.kind == TokenKindOp) {
    if (!str_eq(token.str, STR_LIT("&"))) {
      PERROR("%s:%d:%d: ", "Unexpected ",
             parser->file_path, token.row, token.col);
      print_token_kind(token.kind);
      fputs(", expected &\n", stderr);
      exit(1);
    }

    TypeExprPtr *ptr = aalloc(sizeof(TypeExprPtr));
    ptr->points_to = parser_parse_type_expr(parser);
    return (TypeExpr) { TypeExprKindPtr, { .ptr = ptr } };
  }

  ERROR("Unreachable\n");
  exit(1);
}

static Expr parser_parse_lhs(Parser *parser);
static Expr parser_parse_expr(Parser *parser, i32 min_precedence);
static Expr parser_parse_block(Parser *parser, TokenKind sep, TokenKind end_with);

static Expr parser_parse_let(Parser *parser) {
  Args args = {0};
  bool is_func = false;
  bool has_type = false;
  TypeExpr type = { TypeExprKindUnit };
  Token name = parser_expect_token(parser, TokenKindIdent | TokenKindOp);
  Token token = parser_expect_token(parser,
                                    TokenKindOParen | TokenKindOp |
                                    TokenKindColon);

  if (token.kind == TokenKindOParen) {
    is_func = true;

    while (parser_peek_token(*parser, 1).kind != TokenKindCParen) {
      Arg arg = {0};
      arg.name = parser_expect_token(parser, TokenKindIdent).str;
      parser_expect_token(parser, TokenKindColon);
      arg.type = parser_parse_type_expr(parser);
      DA_APPEND(args, arg);

      if (parser_peek_token(*parser, 1).kind != TokenKindCParen)
        parser_expect_token(parser, TokenKindComma);
    }

    parser_next_token(parser);
    token = parser_expect_token(parser, TokenKindOp | TokenKindColon);
  }

  if (token.kind == TokenKindColon) {
    has_type = true;
    type = parser_parse_type_expr(parser);
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

  if (is_func) {
    expr.kind = ExprKindFunc;
    expr.as.func = aalloc(sizeof(ExprFunc));
    expr.as.func->name = name.str;
    expr.as.func->args = args;
    expr.as.func->body = value;
    expr.as.func->result_type = type;
  } else {
    expr.kind = ExprKindVar;
    expr.as.var = aalloc(sizeof(ExprVar));
    expr.as.var->name = name.str;
    expr.as.var->value = value;
    expr.as.var->has_type = has_type;
    expr.as.var->type = type;
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

  Token token = parser_peek_token(*parser, 2);
  if (token.kind == TokenKindElse) {
    parser_expect_token(parser, TokenKindSemi);
    parser_next_token(parser);
    parser_expect_token(parser, TokenKindColon);
    eef.as.eef->elze = parser_parse_expr(parser, 0);
    eef.as.eef->has_else = true;
  } else if (token.kind == TokenKindElif) {
    parser_expect_token(parser, TokenKindSemi);
    parser_next_token(parser);
    eef.as.eef->elze = parser_parse_if(parser);
    eef.as.eef->has_else = true;
  }

  return eef;
}

static Expr parser_parse_while(Parser *parser) {
  Expr whail;

  whail.kind = ExprKindWhile;
  whail.as.whail = aalloc(sizeof(ExprWhile));
  whail.as.whail->cond = parser_parse_expr(parser, 0);
  parser_expect_token(parser, TokenKindColon);
  whail.as.whail->body = parser_parse_expr(parser, 0);

  return whail;
}

static Expr parser_parse_ret(Parser *parser) {
  Expr ret;

  ret.kind = ExprKindRet;
  ret.as.ret = aalloc(sizeof(ExprRet));
  ret.as.ret->has_result = parser_peek_token(*parser, 0).kind != TokenKindSemi;
  if (ret.as.ret->has_result)
    ret.as.ret->result = parser_parse_expr(parser, 0);

  return ret;
}

static Expr parser_parse_asm(Parser *parser) {
  Expr _asm;
  _asm.kind = ExprKindAsm;
  _asm.as._asm = aalloc(sizeof(Expr));
  _asm.as._asm->nodes = NULL;

  AsmNode *nodes_end = NULL;
  while (parser_peek_token(*parser, 1).kind != TokenKindSemi) {
    LL_PREPEND(_asm.as._asm->nodes, nodes_end, AsmNode);
    Token token = parser_expect_token(parser, TokenKindStrLit | TokenKindIdent);
    if (token.kind == TokenKindStrLit) {
      nodes_end->expr.kind = ExprKindLit;
      nodes_end->expr.as.lit = aalloc(sizeof(ExprLit));
      nodes_end->expr.as.lit->kind = LitKindStr;
      nodes_end->expr.as.lit->lit = token.str;
    } else if (token.kind == TokenKindIdent) {
      nodes_end->expr.kind = ExprKindIdent;
      nodes_end->expr.as.ident = aalloc(sizeof(ExprIdent));
      nodes_end->expr.as.ident->ident = token.str;
    }
  }

  nodes_end->next = NULL;

  return _asm;
}

static Expr parser_parse_use(Parser *parser) {
  Token file_token = parser_expect_token(parser, TokenKindStrLit);

  char *file_path = aalloc(file_token.str.len + 1);
  memcpy(file_path, file_token.str.ptr, file_token.str.len);
  file_path[file_token.str.len] = '\0';
  Str content = read_file(file_path);
  Expr program = parse_program(content, file_path);

  Expr use;
  use.kind = ExprKindUse;
  use.as.use = aalloc(sizeof(ExprUse));
  use.as.use->content = program.as.block;

  return use;
}

static Expr parser_parse_array(Parser *parser) {
  Expr block = parser_parse_block(parser, TokenKindComma, TokenKindCSqr);
  Expr array;
  array.kind = ExprKindArray;
  array.as.array = aalloc(sizeof(ExprArray));
  array.as.array->elements = block.as.block;

  return array;
}

static Expr parser_parse_lhs(Parser *parser) {
  Expr lhs;
  Token token = parser_peek_token(*parser, 1);

  if (token.kind == TokenKindOp) {
    parser_next_token(parser);

    lhs.kind = ExprKindCall;
    lhs.as.call = aalloc(sizeof(ExprCall));
    lhs.as.call->name = token.str;
    lhs.as.call->args = aalloc(sizeof(ExprBlock));
    *lhs.as.call->args = (ExprBlock) {
      .items = aalloc(sizeof(Expr)),
      .len = 1,
      .cap = 1,
    };
    lhs.as.call->args->items[0] = parser_parse_lhs(parser);

    return lhs;
  }

  token = parser_expect_token(parser,
                              TokenKindIntLit | TokenKindIdent |
                              TokenKindOParen | TokenKindStrLit |
                              TokenKindLet | TokenKindIf |
                              TokenKindWhile | TokenKindRet |
                              TokenKindAsm | TokenKindUse |
                              TokenKindOSqr);

  _Static_assert(TokenKindCount == 20, "All token kinds should be handled here.");
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
    if (parser_peek_token(*parser, 1).kind == TokenKindOParen) {
      parser_next_token(parser);

      Expr args = parser_parse_block(parser, TokenKindComma, TokenKindCParen);

      lhs.kind = ExprKindCall;
      lhs.as.call = aalloc(sizeof(ExprCall));
      lhs.as.call->name = token.str;
      lhs.as.call->args = args.as.block;
    } else {
      lhs.kind = ExprKindIdent;
      lhs.as.ident = aalloc(sizeof(ExprIdent));
      lhs.as.ident->ident = token.str;
    }
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
  } else if (token.kind == TokenKindWhile) {
    lhs = parser_parse_while(parser);
  } else if (token.kind == TokenKindRet) {
    lhs = parser_parse_ret(parser);
  } else if (token.kind == TokenKindAsm) {
    lhs = parser_parse_asm(parser);
  } else if (token.kind == TokenKindUse) {
    lhs = parser_parse_use(parser);
  } else if (token.kind == TokenKindOSqr) {
    lhs = parser_parse_array(parser);
  }

  while (parser_peek_token(*parser, 1).kind == TokenKindOSqr) {
    parser_next_token(parser);

    ExprDeref *deref = aalloc(sizeof(ExprDeref));
    deref->body = lhs;
    deref->index = parser_parse_expr(parser, 0);
    lhs.kind = ExprKindDeref;
    lhs.as.deref = deref;

    parser_expect_token(parser, TokenKindCSqr);
  }

  return lhs;
}

static Expr parser_parse_expr(Parser *parser, i32 min_precedence) {
  Expr lhs = parser_parse_lhs(parser);
  ExprCall *expr = NULL;

  for (;;) {
    Token token = parser_peek_token(*parser, 1);
    i32 precedence = op_precedence(token.str);
    if (token.kind != TokenKindOp || precedence <= min_precedence)
      break;
    parser_next_token(parser);

    Expr rhs = parser_parse_expr(parser, precedence);

    expr = aalloc(sizeof(ExprCall));
    expr->name = token.str;
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

  while (parser_peek_token(*parser, 1).kind != end_with) {
    Expr expr = parser_parse_expr(parser, 0);
    DA_APPEND(*block, expr);

    if (parser_peek_token(*parser, 1).kind != end_with)
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
