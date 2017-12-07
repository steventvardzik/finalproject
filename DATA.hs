module DATA where

-- Where all my data should eventually go

data Or l r = Or {
	leftFormula :: l,
	rightFormula :: r
	}

data Variable n t = Variable {
	v_name :: n,
	v_time :: t
}

data Predicate n t = Predicate {
	p_name :: n,
	p_terms :: t
}

data Not f = Not {
	formula :: f
}

data UnificationTerm n t = UnificationTerm {
	u_name :: n,
	u_time :: t
}

data Function a b c = Function {
	f_name :: a,
	f_terms :: b,
	f_time :: c
}
