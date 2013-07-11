package leachi

case class Gene(name: String) extends AnyVal

case class GeneExpression(
  gene: Gene,
  expression: Map[Int, Double],
  infection: Infection,
  activation: Activation)

case class GeneSet(name: String, dataBase: String, set: Set[Gene])

case class EnrichmentResult(logP: Double,
  qVal: Double,
  countInBackground: Double,
  expectedCount: Double,
  countInCluster: Double,
  sourceURL: String,
  cluster: GeneSet,
  predefinedSet: GeneSet,
  direction: String,
  database: String)

