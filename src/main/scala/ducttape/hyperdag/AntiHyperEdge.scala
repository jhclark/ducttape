package ducttape.hyperdag

class AntiHyperEdge[H,E](id: Int, h: H, e: Seq[E])
  extends HyperEdge[H,E](id, h, e);