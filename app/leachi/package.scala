import scala.io.Source
import mybiotools.readTableAsMap
import mybiotools.readTable

import mybiotools.stringstore._
import language.implicitConversions

package object leachi {

  implicit def tup2spec2(x: Tuple2[Int, Float]): Spec2 = Spec2(x._1, x._2)

  case class GeneFormObject(
      idList: String,
      aZA: Boolean,
      sAHA: Boolean,
      dISU: Boolean,
      cD3: Boolean,
      dMSO: Boolean,
      iL7: Boolean,
      hiv: Boolean,
      mock: Boolean) {
    def activatorList: List[Activation] = List((AZA, aZA),
      (SAHA, sAHA),
      (DMSO, dMSO),
      (DISU, dISU),
      (CD3, cD3),
      (IL7, iL7)).filter(_._2).map(_._1)

    def infectionList = List((HIV, hiv), (Mock, mock)).filter(_._2).map(_._1)

  }

  sealed trait Infection
  case object HIV extends Infection
  case object Mock extends Infection

  sealed trait Activation
  case object Resting extends Activation
  case object CD3 extends Activation
  case object IL7 extends Activation
  case object DMSO extends Activation
  case object SAHA extends Activation
  case object AZA extends Activation
  case object DISU extends Activation

  def readExpressionFile(expr: Source): Vector[GeneExpression] = {
    val it = expr.getLines
    val head = it.next.split(" ").map(_.stripPrefix("\"").stripSuffix("\"")).toIndexedSeq

    // "mockW0" "HIVW0" "mockW2" "HIVW2" "mockW4" "HIVW4" "mockW6" "HIVW6" "mockW8" "HIVW8" "mockW10" "HIVW10" "DMSO.8H" "DMSO.24H" "SAHA.8H" "SAHA.24H" "DISU.8H" "DISU.24H" "AZA.8H" "AZA.24H" "IL7.8H" "IL7.24H" "CD3.8H" "CD3.24H" "CD3.72H" "DMSO.8Hmock" "DMSO.24Hmock" "SAHA.8Hmock" "SAHA.24Hmock" "DISU.8Hmock" "DISU.24Hmock" "AZA.8Hmock" "AZA.24Hmock" "IL7.8Hmock" "IL7.24Hmock" "CD3.8Hmock" "CD3.24Hmock" "CD3.72Hmock" "H80rep1" "H80rep2"

    it.toVector.flatMap { line =>
      val spl = mybiotools.fastSplitSeparator(line, ' ')
      val geneName = Gene.fromIntern(StringStore(spl(0).stripPrefix("\"").stripSuffix("\"")))

      val data: Map[String, Float] = (head zip (spl.drop(1).map(_.toFloat))).toMap

      Vector(
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW0"),
            2 -> data("mockW2"),
            4 -> data("mockW4"),
            6 -> data("mockW6"),
            8 -> data("mockW8"),
            10 -> data("mockW10")
          ),
          infection = Mock,
          activation = Resting
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW0"),
            2 -> data("HIVW2"),
            4 -> data("HIVW4"),
            6 -> data("HIVW6"),
            8 -> data("HIVW8"),
            10 -> data("HIVW10")
          ),
          infection = HIV,
          activation = Resting
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("DMSO.8H"),
            24 -> data("DMSO.24H")
          ),
          infection = HIV,
          activation = DMSO
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("SAHA.8H"),
            24 -> data("SAHA.24H")
          ),
          infection = HIV,
          activation = SAHA
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("DISU.8H"),
            24 -> data("DISU.24H")
          ),
          infection = HIV,
          activation = DISU
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("AZA.8H"),
            24 -> data("AZA.24H")
          ),
          infection = HIV,
          activation = AZA
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("IL7.8H"),
            24 -> data("IL7.24H")
          ),
          infection = HIV,
          activation = IL7
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("HIVW10"),
            8 -> data("CD3.8H"),
            24 -> data("CD3.24H")
          ),
          infection = HIV,
          activation = CD3
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("DMSO.8Hmock"),
            24 -> data("DMSO.24Hmock")
          ),
          infection = Mock,
          activation = DMSO
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("SAHA.8Hmock"),
            24 -> data("SAHA.24Hmock")
          ),
          infection = Mock,
          activation = SAHA
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("DISU.8Hmock"),
            24 -> data("DISU.24Hmock")
          ),
          infection = Mock,
          activation = DISU
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("AZA.8Hmock"),
            24 -> data("AZA.24Hmock")
          ),
          infection = Mock,
          activation = AZA
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("IL7.8Hmock"),
            24 -> data("IL7.24Hmock")
          ),
          infection = Mock,
          activation = IL7
        ),
        GeneExpression(
          gene = geneName,
          expression = List(
            0 -> data("mockW10"),
            8 -> data("CD3.8Hmock"),
            24 -> data("CD3.24Hmock")
          ),
          infection = Mock,
          activation = CD3
        )

      )
    }
  }

  def readGeneSets(geneSetFile: Source, dbName: String8): Vector[GeneSet] = {
    geneSetFile.getLines.map { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      val name: String8 = StringStore(new String(spl.head)) //.replaceAll("\""," ").trim
      // val dbname: String8 = StringStore(new String(spl(1)))
      val set: Set[GeneWithDEG] = spl.drop(2).map { gene =>
        GeneWithDEG(Gene.fromIntern(StringStore(new String(gene))), None)
      }.toSet

      GeneSet(name,
        dbName,
        set)
    }.toVector
  }

  def readEnrichmentFile(enrichmentFile: Source, filename: String, clusters: Vector[GeneSet], geneSets: Vector[GeneSet]): Vector[EnrichmentResult] = {

    val clusternameprefix = filename.drop(26).dropRight(44)
    enrichmentFile.getLines.drop(1).map { line =>
      val spl = mybiotools.fastSplitSeparator(line, '\t')
      // DataBase  ClusterName SetName Over/UnderRep.  log10(Pval) Qval  CountinBackground ExpectedCount CountinCluster  SourceUrl ClusterID theGenes
      val logp = spl(4) match {
        case x if x == "-Inf" => Float.MinValue
        case x if x == "Inf" => Float.MaxValue
        case x => x.toFloat
      }
      val qval = spl(5).toFloat
      val countInBackground = spl(6).toInt
      val expectedcount = spl(7).toFloat
      val countInCluster = spl(8).toInt
      val source = spl(9)
      val clustername = clusternameprefix + "-" + spl(1)
      val db = spl(0)
      val setname = spl(2)
      val direction = spl(3)
      val predefinedSet = geneSets.filter(_.name.value == setname).headOption
      val cluster = clusters.filter(_.name.value == clustername).headOption
      if (predefinedSet.isDefined && cluster.isDefined)
        Some(EnrichmentResult(
          logP = logp,
          qVal = qval,
          countInBackground = countInBackground,
          expectedCount = expectedcount,
          countInCluster = countInCluster,
          sourceURL = new String(source),
          cluster = cluster.get,
          predefinedSet = predefinedSet.get,
          direction = new String(direction),
          database = new String(db)))
      else None
    }.filter(_.isDefined).map(_.get).toVector

  }

  def readClusterFiles(in: Map[String, io.Source]): Vector[GeneSet] = {
    in.map {
      case (name, source) =>
        GeneSet(name = StringStore(new String(name.dropRight(4))), dataBase = StringStore("diffexp"), set = source.getLines.map { x =>
          val spl = x.split(",")
          val name = spl(0).stripPrefix("\"").stripSuffix("\"")
          val fold = spl(1).stripPrefix("\"").stripSuffix("\"").toFloat
          val p = spl(2).stripPrefix("\"").stripSuffix("\"").toFloat
          val padj = spl(3).stripPrefix("\"").stripSuffix("\"").toFloat
          GeneWithDEG(Gene.fromIntern(StringStore(name)), Some(DEG(fold, p, padj)))
        }.toSet)
    }.toVector
  }

}