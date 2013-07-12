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

  def ping = Action {
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
      Ok(views.html.index(GeneData.clusters, ClusterController.clusterSelectForm, GenesController.geneInputForm, GenesController.geneSetQueryForm))
    }
  }

  //   // GET /listgenesets/:text
  //   def listGeneSets( text: String ) = Action {
  //     val gs = geneSetsFromText( text )
  //     Ok( views.html.geneSetList( gs.mapValues(gs => gs -> whichClustersAreEnrichedInGeneSet(gs)), geneSetQueryForm ) )
  //   }

  def getImageFuture(genes: Traversable[GeneExpression], name: String): Future[String] = {
    Future {
      val factory = DrawableWriterFactory.getInstance();
      val writer = factory.get("image/png");
      val plot = plots.createTimeLinePlot(genes, name)
      val bs = new ByteArrayOutputStream()
      writer.write(plot, bs, 900, 300);

      DatatypeConverter.printBase64Binary(bs.toByteArray)
    }
  }

  //   private def geneSetsFromText( t: String ) = {
  //     val ids = mybiotools.fastSplitSetSeparator( t, SeparatorCharacters ).distinct.map( _.toUpperCase )
  //     GeneData.predefinedGeneSets.filter( x => ids.contains( x._1.toUpperCase ) )
  //   }

  // private val CacheExpiryTime = current.configuration.getInt( "hiv24.cacheExpiryInSec" ).getOrElse(60*60)

}

