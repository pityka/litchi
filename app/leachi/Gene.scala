package leachi

import mybiotools.stringstore._

case class Gene(name: String8)

object Gene {
  val intern = collection.mutable.Map[String8, Gene]()

  def fromIntern(name: String8) = {
    intern.get(name) match {
      case None =>
        {
          val x = Gene(name);
          intern.update(name, x);
          x
        }
      case Some(y) => y
    }
  }

}

case class Spec2(_1: Int, _2: Float)

case class DEG(foldChange: Float, p: Float, pAdj: Float)

case class GeneWithDEG(gene: Gene, deg: Option[DEG])

case class GeneExpression(
  gene: Gene,
  expression: List[Spec2],
  infection: Infection,
  activation: Activation)

case class GeneSet(name: String8, dataBase: String8, set: Set[GeneWithDEG])

case class EnrichmentResult(logP: Float,
  qVal: Float,
  countInBackground: Float,
  expectedCount: Float,
  countInCluster: Float,
  sourceURL: String,
  cluster: GeneSet,
  predefinedSet: GeneSet,
  direction: String,
  database: String)

