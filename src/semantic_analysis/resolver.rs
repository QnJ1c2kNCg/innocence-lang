// The [`Resolver`] is responsible for variable resolution as part of the semantic passes.
// The goal here is to avoid our [`Interpreter`] to have to resolve every variable dynamically
// at runtime. For instance, if a variable is used in a loop, our [`Resolver`] will visit that
// loop node of the AST once, instead of having our [`Interpreter`] resolve the variable for
// each loop iteration.
//
// [12-6-2025]: I'm not sure this is needed for innocence, since performance is not a goal.
