package controllers

import play.api._
import play.api.mvc._
import models.GeneData
import leachi._
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import java.io.ByteArrayOutputStream
import play.api.libs.concurrent.{ Promise, Akka }
import javax.xml.bind.DatatypeConverter
import play.api.Play.current
import play.api.data.Forms._
import play.api.data._
import play.api.cache.{ Cache, Cached }

import scala.concurrent._
import ExecutionContext.Implicits.global

import GenesController.geneInputForm

object ClusterController extends Controller {

  val clusterSelectForm = Form(
    tuple(
      "clusterName" -> text,
      "activator1" -> optional(text),
      "activator2" -> optional(text)))

  // GET /cluster/
  def showClusterFromForm = Cached(request => request.toString) {
    Action { implicit request =>
      clusterSelectForm.bindFromRequest.fold(
        errors => { BadRequest },
        cluster => {

          val (ac1, ac2) = if (cluster._2.isEmpty && cluster._3.isEmpty) {
            val activators = getActivatorsFromClusterName(cluster._1)
            (activators.head, activators.drop(1).headOption.getOrElse(activators.head))
          } else (GenesController.activatorFromString(cluster._2), GenesController.activatorFromString(cluster._3))

          val cl = GeneData.geneSetsByName.get(cluster._1)
          cl.map(showClusterHelper(_, true,
            ac1,
            ac2
          )).getOrElse(BadRequest)
        })
    }
  }

  // GET /cluster/:ID
  def showCluster(name: String) = Action {
    val activators = getActivatorsFromClusterName(name)
    GeneData.geneSetsByName.get(name).map(showClusterHelper(_, true, activators.head, activators.drop(1).head)).getOrElse(BadRequest)

  }

  def showGeneSet(name: String) = Action {
    val decodedName = java.net.URLDecoder.decode(name, "utf-8")
    GeneData.geneSetsByName.get(decodedName).map(showClusterHelper(_, false, DMSO, DMSO)).getOrElse(Ok(views.html.emptyPage()))
  }

  def csvCluster(name: String) = Action {
    GeneData.geneSetsByName.get(name).map(serveCSV).getOrElse(BadRequest)
  }

  private def serveCSV(cluster: GeneSet): Result = {
    val genes = cluster.set

    SimpleResult(
      header = ResponseHeader(200, Map("Content-Disposition" -> "attachment; filename=table.txt")),
      body = play.api.libs.iteratee.Enumerator(renderCSV(genes)))
  }

  private def renderCSV(genes: Traversable[Gene]) = genes.map(g => List(g.name).mkString(",")).mkString("\n")

  private def showClusterHelper(cluster: GeneSet, searchInClusters: Boolean, activator1: Activation, activator2: Activation): Result = {
    val genes = GeneData.genesByCluster(cluster)

    val geneExpressionData: Vector[GeneExpression] = genes.map(x => GeneData.expressionsByGene.get(x)).filter(_.isDefined).map(_.get).toVector.flatten.filter(x => List(Resting, activator1, activator2).contains(x.activation) && (x.infection == HIV))

    val promiseOfImage: Future[Option[String]] = if (genes.size > 0) {
      play.api.cache.Cache.get(cluster.name + activator1.toString + activator2.toString) match {
        case Some(x) => Promise.pure(Some(x.asInstanceOf[String]))
        case None => Application.getImageFuture(geneExpressionData, cluster.name.toString).map(x => Some(x))
      }
    } else {
      Promise.pure(None)
    }

    val enrichmentResults: Vector[EnrichmentResult] = if (searchInClusters) GeneData.enrichmentTestsByClusterName.get(cluster.name).getOrElse(Vector())
    else GeneData.enrichmentTestsByGeneSetName.get(cluster.name).getOrElse(Vector())

    Async {
      promiseOfImage.map { (image: Option[String]) =>
        image.foreach(x => play.api.cache.Cache.set(cluster.name + activator1.toString + activator2.toString, x))
        Ok(views.html.showClusterPage(genes, image, cluster.name, enrichmentResults, bindGenesToClusterForm(cluster, activator1, activator2), routes.ClusterController.showClusterFromForm))
      }

    }

  }

  private def getActivatorsFromClusterName(name: String): List[Activation] = {
    val f1 = name.split("-").head
    f1.split("vs").map(x => GenesController.activatorFromString(Some(x))).toList
  }

  def bindGenesToClusterForm(cluster: GeneSet, activator1: Activation, activator2: Activation) = clusterSelectForm.bind(Map(
    "clusterName" -> cluster.name,
    ("activator1" -> (activator1 match {
      case AZA => "AZA"
      case CD3 => "CD3"
      case IL7 => "IL7"
      case DMSO => "DMSO"
      case SAHA => "SAHA"
      case DISU => "DISU"
      case _ => ""
    })),
    ("activator2" -> (activator2 match {
      case AZA => "AZA"
      case CD3 => "CD3"
      case IL7 => "IL7"
      case DMSO => "DMSO"
      case SAHA => "SAHA"
      case DISU => "DISU"
      case _ => ""
    }))
  ))

}