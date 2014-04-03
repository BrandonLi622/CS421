structure Ford =
    struct
        type car = {make : string, built : int}
        val first = {make = "Ford", built = 1904}
        fun mutate (c : car) year = 
            {make = #make c, built = year}
        fun built (c : car) = #built c
        fun show (c) = if (built c) < (built first)
                       then " = " else "(generic Ford)"
    end;
