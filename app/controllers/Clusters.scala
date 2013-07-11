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
      "activator" -> optional(text)))

  // GET /cluster/
  def showClusterFromForm = Cached(request => request.toString) {
    Action { implicit request =>
      clusterSelectForm.bindFromRequest.fold(
        errors => { println(errors); BadRequest },
        cluster => {
          val cl = GeneData.geneSetsByName.get(cluster._1)
          cl.map(showClusterHelper(_, true, GenesController.activatorFromString(cluster._2))).getOrElse(BadRequest)
        })
    }
  }

  // GET /cluster/:ID
  def showCluster(name: String) = Action {
    GeneData.geneSetsByName.get(name).map(showClusterHelper(_, true, DMSO)).getOrElse(BadRequest)

  }

  def showGeneSet(name: String) = Action {
    GeneData.geneSetsByName.get(name).map(showClusterHelper(_, false, DMSO)).getOrElse(BadRequest)

  }

  private def showClusterHelper(cluster: GeneSet, searchInClusters: Boolean, activator: Activation): Result = {
    val genes = GeneData.genesByCluster(cluster)

    val geneExpressionData: Vector[GeneExpression] = genes.map(x => GeneData.expressionsByGene.get(x)).filter(_.isDefined).map(_.get).toVector.flatten.filter(x => List(DMSO, Resting, activator).contains(x.activation))

    val promiseOfImage: Future[Option[String]] = if (genes.size > 0) {
      play.api.cache.Cache.get(cluster.name + activator.toString) match {
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
        image.foreach(x => play.api.cache.Cache.set(cluster.name + activator.toString, x))
        Ok(views.html.showGenesPage(genes, image, cluster.name, enrichmentResults, bindGenesToClusterForm(cluster, activator), routes.ClusterController.showClusterFromForm))
      }

    }

  }

  def bindGenesToClusterForm(cluster: GeneSet, activator: Activation): Form[(String, Option[String])] = clusterSelectForm.bind(Map("clusterName" -> cluster.name, ("activator" -> (activator match {
    case AZA => "AZA"
    case CD3 => "CD3"
    case IL7 => "IL7"
    case DMSO => "DMSO"
    case SAHA => "SAHA"
    case DISU => "DISU"
    case _ => ""
  }))))

}