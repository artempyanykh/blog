constant m : nat
constant n : nat → nat

#check m
#check n m
#check n m + 0

#check Type
#check Prop

#check list
#check prod

#check fun x : nat, x + 5

#reduce (fun x, x + 5 ) 5

def fib : ℕ → ℕ
| 0 := 1
| 1 := 1
| (n + 2) := fib n  + fib (n + 1)

section
    variables {α β γ : Type}
    variable x: α

    def curry (f: α × β → γ) : α → β → γ := λ x y, f (x, y)

    #check curry

    def foo (tup : ℕ × ℕ) := tup.1 + tup.2 

    #check curry foo

    #eval (curry foo) 1 2

    def id' := fun x : Type, x

    #check id

    #check foo

    #eval foo (1, 2)
end

-- Π is a dependent function type
-- Σ is a dependent product type

def bar {α : Type} (f: α -> ℕ) (x: α): ℕ := f x

#eval bar (λ x, x + 5) 5