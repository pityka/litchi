package models

import play.api.Play.current
import scala.io.Source
import leachi._
import java.io.File
import mybiotools._
import stringstore._

object GeneData {

  private val expressionsFile = Source.fromURL(getClass.getResource(current.configuration.getString("leachi.geneExpressionsFile").get))

  private val clustersFile = Source.fromURL(getClass.getResource(current.configuration.getString("leachi.clusterListFile").get))

  private val enrichmentTestsFiles = play.api.Configuration.unapply(current.configuration).get.getStringList("leachi.enrichmentTestsFiles").toArray.toList.asInstanceOf[List[String]].map { x =>
    x -> Source.fromURL(getClass.getResource(x))
  }

  private val geneSetFiles = play.api.Configuration.unapply(current.configuration).get.getStringList("leachi.predefinedGeneSets").toArray.toList.asInstanceOf[List[String]].map { x =>
    x -> Source.fromURL(getClass.getResource(x))
  }

  lazy val clusters: Vector[GeneSet] = readClusterFiles(clustersFile.getLines.map { file =>

    file -> Source.fromURL(getClass.getResource("/inputfiles/Clusters/" + file))
  }.toMap)

  lazy val (expressionsByGene: Map[Gene, Vector[GeneExpression]], genes) = {
    val expressions: Vector[GeneExpression] = readExpressionFile(expressionsFile)
    (expressions.groupBy(_.gene).toMap, expressions.map(_.gene).distinct)
  }

  def genesByCluster(cl: GeneSet) = cl.set

  lazy val predefinedGeneSets: Vector[GeneSet] = geneSetFiles.flatMap { x =>
    val geneSetFile = x._2
    val dbname = StringStore(new File(x._1).getName)
    readGeneSets(geneSetFile, dbname)
  }.toVector

  lazy val enrichmentTests: Vector[EnrichmentResult] = enrichmentTestsFiles.map(x => readEnrichmentFile(x._2, new File(x._1).getName, clusters, predefinedGeneSets)).reduce(_ ++ _)

  lazy val enrichmentTestsByClusterName: Map[String8, Vector[EnrichmentResult]] = enrichmentTests.groupBy(_.cluster.name).toMap

  lazy val enrichmentTestsByGeneSetName: Map[String8, Vector[EnrichmentResult]] = enrichmentTests.groupBy(_.predefinedSet.name).toMap

  lazy val geneSetsByName: Map[String8, GeneSet] = (clusters ++ predefinedGeneSets).map(x => x.name -> x).toMap

  lazy val clusterByGene: Map[Gene, Set[GeneSet]] = clusters.map(cl => cl.set.map(g => g.gene -> Set(cl)).toMap).reduce((x, y) => mybiotools.addMaps(x, y)(_ ++ _))

  val listOfActivatorNames = List("TCR", "IL7", "AZA", "DISU", "SAHA", "DMSO")

}