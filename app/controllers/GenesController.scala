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
import scala.concurrent.duration._

import scala.concurrent._
import ExecutionContext.Implicits.global

import GenesController.geneInputForm

object GenesController extends Controller {

  val geneInputForm = Form(
    mapping(
      "idList" -> text,
      "AZA" -> boolean,
      "SAHA" -> boolean,
      "DISU" -> boolean,
      "TCR" -> boolean,
      "DMSO" -> boolean,
      "IL7" -> boolean,
      "HIV" -> boolean,
      "Mock" -> boolean)(GeneFormObject.apply)(GeneFormObject.unapply)
  )

  val geneSetQueryForm = Form(
    "keywords" -> optional(text))

  // POST /genes
  def listGenesFromForm = Action.async { implicit request =>
    geneInputForm.bindFromRequest.fold(
      errors => Future.successful(BadRequest),
      (tuple: GeneFormObject) => {

        showGenesHelper(geneSetFromString(tuple.idList), if (tuple.activatorList.isEmpty) List(DMSO, Resting) else tuple.activatorList, if (tuple.infectionList.isEmpty) List(Mock, HIV) else tuple.infectionList)

      })
  }

  // GET /genespng/:gene
  def genePNG(list: String) = Action.async { implicit request =>
    val genes = geneSetFromString(list)
    showGenesPNG(genes, List(DMSO, CD3, IL7, AZA, DISU, SAHA), List(HIV))
  }

  // GET /genes/:list
  def listGenes(list: String, activator: String) = Action.async { implicit request =>
    val genes = geneSetFromString(list)
    showGenesHelper(genes, List(activatorFromString(Some(activator))), List(HIV))
  }

  private def showGenesHelper(genes: Traversable[Gene], activators: List[Activation], infection: List[Infection])(implicit request: Request[_]): Future[SimpleResult] = {
    if (genes.size > 0) {

      val geneExpressionData: Vector[GeneExpression] = genes.map(x => GeneData.expressionsByGene.get(x)).filter(_.isDefined).map(_.get).toVector.flatten.filter(x => (Resting :: activators).contains(x.activation) && infection.contains(x.infection))

      val promiseOfImage = Application.getImageFuture(geneExpressionData, "", cacheResult = false)

      promiseOfImage.map { (image: String) =>
        Ok(views.html.showOneGene(genes, Some(image), genes.map(_.name).mkString(" "), bindGenesToForm(genes, activators, infection)))
      }

    } else {
      Future.successful(NotFound(views.html.emptyPage()))
    }
  }

  private def showGenesPNG(genes: Traversable[Gene], activators: List[Activation], infection: List[Infection])(implicit request: Request[_]): Future[SimpleResult] = {
    if (genes.size > 0) {

      val geneExpressionData: Vector[GeneExpression] = genes.map(x => GeneData.expressionsByGene.get(x)).filter(_.isDefined).map(_.get).toVector.flatten.filter(x => (Resting :: activators).contains(x.activation) && infection.contains(x.infection))

      val promiseOfImage = Application.getImageFutureBinary(geneExpressionData, genes.head.name.value, cacheResult = false)

      model.TimeoutFuture(25 seconds)(promiseOfImage.map {
        image => Ok(image).as("image/png")
      }).recover({
        case _: Throwable => InternalServerError
      })

    } else {
      Future.successful { NotFound }
    }
  }

  // GET 	/listgeneset
  def showAllGeneSets = Action {
    Ok(views.html.geneSetList(GeneData.predefinedGeneSets.map(gs => gs -> GeneData.enrichmentTestsByGeneSetName.get(gs.name)).filter(_._2.isDefined).map(x => (x._1, x._2.get)), geneSetQueryForm))
  }

  // GET /listgenesets/
  def queryGeneSets = Action { implicit request =>
    geneSetQueryForm.bindFromRequest.fold(
      errors => BadRequest,
      keywordstextOption => {
        keywordstextOption match {
          case None => Redirect(routes.GenesController.showAllGeneSets)
          case Some(keywordstext) => {
            val keywords = mybiotools.fastSplitSetSeparator(keywordstext, SeparatorCharacters).distinct.map(_.toUpperCase)

            val selectedGeneSets = GeneData.predefinedGeneSets.filter(x => keywords.exists(y => x.name.toUpperCase.indexOf(y) != -1))

            Ok(views.html.geneSetList(selectedGeneSets.map(gs => gs -> GeneData.enrichmentTestsByGeneSetName.get(gs.name)).filter(_._2.isDefined).map(x => (x._1, x._2.get)), geneSetQueryForm))
          }
        }

      })
  }

  private def geneSetFromString(text: String): Set[Gene] = {
    val ids = mybiotools.fastSplitSetSeparator(text, SeparatorCharacters).distinct.map(_.toUpperCase)
    ids.map(x => GeneData.genes.find(y => y.name.toUpperCase == x)).filter(_.isDefined).map(_.get).toSet
  }

  def activatorFromString(text: Option[String]): Activation = text match {
    case Some("CD3") => CD3
    case Some("IL7") => IL7
    case Some("AZA") => AZA
    case Some("DISU") => DISU
    case Some("SAHA") => SAHA
    case _ => DMSO
  }

  private val SeparatorCharacters = Set(':', ',', ';', '\n', '\r', 13.toByte.toChar, 10.toByte.toChar, ' ')

  private def bindGenesToForm(genes: Traversable[Gene], activators: List[Activation], infections: List[Infection]): Form[GeneFormObject] = {
    val gfo = GeneFormObject(
      idList = genes.map(_.name).mkString(":"),
      aZA = activators.contains(AZA),
      dMSO = activators.contains(DMSO),
      dISU = activators.contains(DISU),
      sAHA = activators.contains(SAHA),
      cD3 = activators.contains(CD3),
      iL7 = activators.contains(IL7),
      hiv = infections.contains(HIV),
      mock = infections.contains(Mock)
    )

    geneInputForm.fill(gfo)
  }

}