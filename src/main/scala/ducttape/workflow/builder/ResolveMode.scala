package ducttape.workflow.builder

// Used by resolveNonBranchVar() in TaskTemplateBuilder
// to indicate what kind of spec we're currently resolving
class ResolveMode();
case class InputMode() extends ResolveMode;
case class ParamMode() extends ResolveMode;
case class OutputMode() extends ResolveMode;
