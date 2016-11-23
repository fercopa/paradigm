#include "evaluator.h"
#include "ast.h"
#include "assignment.h"
#include "misc.h"


int eval(ASTNode *f, Asgmnt *a) {
    int res = 0;
    int p = 0,
        q = 0;
    Asgmnt *tmp = NULL;

    if (f != NULL) {
        tmp = calloc(1, sizeof(struct Asgmnt));
        if (tmp != NULL) {
            switch (f->type) {
                case OR:
                    p = (int)(eval(f->l_succ, a));
                    q = (int)(eval(f->r_succ, a));
                    tmp->value = (int)(p || q);
                    break;
                case AND:
                    p = (int)(eval(f->l_succ, a));
                    q = (int)(eval(f->r_succ, a));
                    tmp->value = (int)(p && q);
                    break;
                case IMPL:
                    /* P -> Q =~ not P or Q */
                    p = (int)(eval(f->l_succ, a));
                    q = (int)(eval(f->r_succ, a));
                    tmp->value = (int)(!p || q);
                    break;
                case NOT:
                    tmp->value = (int)(!eval(f->l_succ, a));
                    break;
                case IFF:
                    /* P <-> Q =~ (P -> Q) and (Q -> P) */
                    p = (int)(eval(f->l_succ, a));
                    q = (int)(eval(f->r_succ, a));
                    tmp->value = (int)((!p || q) && (!q || p));
                    break;
                case PROP:
                    ASSIGNMENT_FIND(a, &f->id, tmp);
                    break;
                default:
                    printf("Operador no valido.\n");
                    return (-1);
            }
            res = tmp->value;
        }
    }
    return res;
}
