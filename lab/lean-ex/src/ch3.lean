namespace n1
    variables p q : Prop

    theorem t1 : p → q → p := λ hp : p, λ hq : q, hp
    #check t1

    def t1' (α β: Type) : α → β → α := λ a b, a
    #check t1'

    #help commands
    #check list
    #print list
    #eval list ℕ
end n1

namespace n2
    variables p q r s: Prop
    variable  hp : p

    theorem t1 : q → p := λ (hq : q), hp
    #check t1

    variable h : r → s
    #check t1 (r → s) (s → r) h
end n2

namespace n3
    variables p q : Prop

    lemma pand: p → q → (p ∧ q) := assume p q, and.intro p q
end n3