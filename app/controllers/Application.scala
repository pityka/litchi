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

object Application extends Controller {

  val compareForm = Form(
    tuple(
      "up" -> boolean,
      "activator1" -> text,
      "activator2" -> text))

  def ping = Action {
    Ok("ittvagyok")
  }

  def pinghead = Action {
    Ok("ittvagyok")
  }

  def about = Action {
    Ok(views.html.about())
  }

  def links = Action {
    Ok(views.html.links())
  }

  def index = Cached("index") {
    Action {
      // Ok("")
      Ok(views.html.index(GeneData.clusters, compareForm, GenesController.geneInputForm, GenesController.geneSetQueryForm))
    }
  }

  def compareConditions = Cached(request => request.toString) {
    Action { implicit request =>
      compareForm.bindFromRequest.fold(
        errors => BadRequest,
        tuple => {

          val ac1 = tuple._2
          val ac2 = tuple._3
          val upInFirst = if (!tuple._1) "Up_in" else "Dn_in"
          val dnInFirst = if (!tuple._1) "Dn_in" else "Up_in"

          val forgedNames = List(
            s"${ac1}vs${ac2}-${upInFirst}-${ac2}-activated",
            s"${ac2}vs${ac1}-${dnInFirst}-${ac1}-activated",
            s"${ac1}vs${ac2}-${dnInFirst}-${ac1}-activated",
            s"${ac2}vs${ac1}-${upInFirst}-${ac2}-activated")

          GeneData.clusters.map(_.name.value).filter { clustername =>

            forgedNames.contains(clustername)

          }
            .headOption.map(x => Redirect(routes.ClusterController.showCluster(x))).getOrElse(Ok(views.html.emptyPage()))
        })
    }
  }

  def getImageFuture(genes: Traversable[GeneExpression], name: String): Future[String] = play.api.cache.Cache.get("image-" + name) match {
    case Some(x) => Promise.pure(x.asInstanceOf[String])
    case None => {
      val f = (Future {
        val factory = DrawableWriterFactory.getInstance();
        val writer = factory.get("image/png");
        val plot = plots.createTimeLinePlot(genes, name)
        val bs = new ByteArrayOutputStream()
        writer.write(plot, bs, 900, 300);

        DatatypeConverter.printBase64Binary(bs.toByteArray)
      })
      f.onSuccess {
        case (s: String) => play.api.cache.Cache.set("image-" + name, s, CacheExpiryTime)
      }
      f
    }
  }

  val CacheExpiryTime = current.configuration.getInt("litchi.cacheExpiryInSec").getOrElse(60 * 60)

}

