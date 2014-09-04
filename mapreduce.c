#include <stdio.h>

/* C-preprocessor used for templates/polymorphic types (with mangled names) */
#define INSTANTIATE_LINKED_LIST(T,suffix) \
struct node_##suffix { \
    T data; struct node_##suffix * next; \
}; \
\
void inplace_map_##suffix(struct node_##suffix * lst, T (*f)(T)) { \
    while(lst != NULL) { \
        lst->data = (*f)(lst->data); \
        lst = lst->next; \
    } \
}

INSTANTIATE_LINKED_LIST(int,int)

int print_and_ident_int(int x)
{
    printf("x: %d\n", x);
    return x;
}

int plus_one_int(int x) { return x+1; }

int main(int argc, char** argv)
{
    struct node_int a,b,c;
    a.data = 0; a.next = &b;
    b.data = 1; b.next = &c;
    c.data = 2; c.next = NULL;
    printf("Before: \n");
    inplace_map_int(&a, &print_and_ident_int);
    inplace_map_int(&a, &plus_one_int);
    printf("After: \n");
    inplace_map_int(&a, &print_and_ident_int);
    return 0;
}
