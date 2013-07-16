package models

import play.api.Play.current
import scala.io.Source
import leachi._
import java.io.File
import mybiotools._

object GeneData {

  private val expressionsFile = Source.fromURL(getClass.getResource(current.configuration.getString("leachi.geneExpressionsFile").get))

  private val clustersFile = Source.fromURL(getClass.getResource(current.configuration.getString("leachi.clusterListFile").get))

  private val enrichmentTestsFiles = play.api.Configuration.unapply(current.configuration).get.getStringList("leachi.enrichmentTestsFiles").toArray.toList.asInstanceOf[List[String]].map { x =>
    x -> Source.fromURL(getClass.getResource(x))
  }

  private val geneSetFiles = play.api.Configuration.unapply(current.configuration).get.getStringList("leachi.predefinedGeneSets").toArray.toList.asInstanceOf[List[String]].map { x =>
    x -> Source.fromURL(getClass.getResource(x))
  }

  val clusters: Vector[GeneSet] = readClusterFiles(clustersFile.getLines.map { file =>

    file -> Source.fromURL(getClass.getResource("/inputfiles/Clusters/" + file))
  }.toMap)

  val expressions: Vector[GeneExpression] = readExpressionFile(expressionsFile)

  val expressionsByGene: Map[Gene, Vector[GeneExpression]] = expressions.groupBy(_.gene).toMap

  val genes = expressions.map(_.gene).distinct

  def genesByCluster(cl: GeneSet) = cl.set

  val predefinedGeneSets: Vector[GeneSet] = geneSetFiles.flatMap { x =>
    val geneSetFile = x._2
    val dbname = new File(x._1).getName
    readGeneSets(geneSetFile, dbname)
  }.toVector

  val enrichmentTests: Vector[EnrichmentResult] = enrichmentTestsFiles.map(x => readEnrichmentFile(x._2, new File(x._1).getName, clusters, predefinedGeneSets)).reduce(_ ++ _)

  val enrichmentTestsByClusterName: Map[String, Vector[EnrichmentResult]] = enrichmentTests.groupBy(_.cluster.name).toMap

  val enrichmentTestsByGeneSetName: Map[String, Vector[EnrichmentResult]] = enrichmentTests.groupBy(_.predefinedSet.name).toMap

  val geneSetsByName: Map[String, GeneSet] = (clusters ++ predefinedGeneSets).map(x => x.name -> x).toMap

  val clusterByGene: Map[Gene, Set[GeneSet]] = clusters.map(cl => cl.set.map(g => g -> Set(cl)).toMap).reduce((x, y) => mybiotools.addMaps(x, y)(_ ++ _))

  val listOfActivatorNames = List("CD3", "IL7", "AZA", "DISU", "SAHA","DMSO")

}