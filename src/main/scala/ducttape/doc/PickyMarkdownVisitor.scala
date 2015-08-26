// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.doc

import org.pegdown.ast._

trait PickyMarkdownVisitor extends Visitor {
  def visit(node: AbbreviationNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: AutoLinkNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: BlockQuoteNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: BulletListNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: CodeNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: DefinitionListNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: DefinitionNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: DefinitionTermNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: EmphNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: ExpImageNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: ExpLinkNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: HeaderNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: HtmlBlockNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: InlineHtmlNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: ListItemNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: MailLinkNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: OrderedListNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: ParaNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: QuotedNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: ReferenceNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: RefImageNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: RefLinkNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: RootNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: SimpleNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: SpecialTextNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: StrongNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableBodyNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableCellNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableColumnNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableHeaderNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: TableRowNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: VerbatimNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: WikiLinkNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }

  def visit(node: TextNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  def visit(node: SuperNode) { throw new RuntimeException("Unsupported markdown element type: " + node.getClass.getName) }
  
  // general catch all for custom Node implementations    
  def visit(node: Node);
}