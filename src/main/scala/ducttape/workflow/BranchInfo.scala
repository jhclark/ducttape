package ducttape.workflow

/**
 * Denotes a branch and any branch grafts associated with that branch.
 * A BranchInfo instance is typically associated with a list of Specs.
 */
class BranchInfo(val branch: Branch, val grafts: Seq[Branch]);