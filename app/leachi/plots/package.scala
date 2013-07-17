package leachi

package object plots {
  import de.erichseifert.gral.data.DataSeries;
  import de.erichseifert.gral.data.DataTable;
  import de.erichseifert.gral.data.filters.Convolution;
  import de.erichseifert.gral.data.filters.Filter;
  import de.erichseifert.gral.data.filters.Kernel;
  import de.erichseifert.gral.data.filters.KernelUtils;
  import de.erichseifert.gral.data.filters.Median;
  import de.erichseifert.gral.plots.Plot;
  import de.erichseifert.gral.plots.XYPlot;
  import de.erichseifert.gral.plots.legends.Legend;
  import de.erichseifert.gral.plots.lines.DefaultLineRenderer2D;
  import de.erichseifert.gral.ui.InteractivePanel;
  import de.erichseifert.gral.util.GraphicsUtils;
  import de.erichseifert.gral.util.Insets2D;
  import de.erichseifert.gral.util.Orientation;
  import de.erichseifert.gral.plots.points._
  import de.erichseifert.gral.plots.lines._
  import de.erichseifert.gral.plots.axes._
  import de.erichseifert.gral.plots.XYPlot.XYPlotArea2D
  import de.erichseifert.gral.graphics.AbstractDrawable
  import de.erichseifert.gral.util._;
  import java.awt._;
  import scala.runtime.RichInt
  import scala.runtime.RichDouble
  import de.erichseifert.gral.graphics.DrawableContainer
  import de.erichseifert.gral.graphics.TableLayout

  val ColorMap = Map[Activation, Color](
    Resting -> Color.gray,
    CD3 -> Color.red,
    IL7 -> Color.cyan,
    DMSO -> Color.gray,
    SAHA -> Color.green,
    AZA -> Color.magenta,
    DISU -> Color.orange
  )

  def createTimeLinePlot(geneexpressions: Traversable[GeneExpression], title: String = ""): AbstractDrawable = {

    def createPlot(dat: Traversable[(Color, scala.collection.immutable.List[Spec2], String)], maxY: Double, minY: Double, title: String, timeUnit: String, legendLocation: Location, legendDistance: Double): XYPlot = {
      if (!dat.isEmpty) {

        val series = dat.map {
          case (color, gene, name) =>
            val ser = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
            gene.toSeq.sortBy(_._1).filter(_._2 > 0.0).foreach { tuple =>
              ser.add(tuple._1.toDouble, math.log10(tuple._2 + 1.0)) / math.log10(2.0)
            }

            (color, ser, name)
        }

        val maxX = dat.map(_._2).map(x => x.map(_._1).toList).flatten.max
        val minX = dat.map(_._2).map(x => x.map(_._1).toList).flatten.min

        val plotHIV = new XYPlot()
        plotHIV.setSetting(Plot.AUTOSCALE_AFTER_ADD, false)
        series.foreach {
          case (color, ser, name) =>
            plotHIV.add(ser)
            val lr = new DefaultLineRenderer2D();
            lr.setSetting(LineRenderer.COLOR, color)
            lr.setSetting(LineRenderer.STROKE, new BasicStroke(1.5f))
            plotHIV.setLineRenderer(ser, lr)
            plotHIV.setPointRenderer(ser, null);

        }
        plotHIV.autoscaleAxes()
        // plotHIV.setLineRenderer(avgserHIV, avgLineRenderer)
        // plotHIV.setPointRenderer(avgserHIV, null);

        // plotHIV.setLineRenderer(upperDashedLine, dashedLineRenderer)
        // plotHIV.setLineRenderer(lowerDashedLine, dashedLineRenderer)
        // plotHIV.setPointRenderer(upperDashedLine, null);
        // plotHIV.setPointRenderer(lowerDashedLine, null);

        val plotarea = plotHIV.getPlotArea
        plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_X, false)
        plotarea.setSetting(XYPlotArea2D.GRID_MAJOR_Y, false)
        plotarea.setSetting(XYPlotArea2D.GRID_MINOR_X, false)
        plotarea.setSetting(XYPlotArea2D.GRID_MINOR_Y, false)

        val rendererY = plotHIV.getAxisRenderer(XYPlot.AXIS_Y);
        rendererY.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
        val rendererX = plotHIV.getAxisRenderer(XYPlot.AXIS_X);
        rendererX.setSetting(AxisRenderer.INTERSECTION, -Double.MaxValue);
        val xAxis = plotHIV.getAxis(XYPlot.AXIS_X)

        xAxis.setRange(minX, maxX)

        val yAxis = plotHIV.getAxis(XYPlot.AXIS_Y)
        yAxis.setRange(math.log10(minY + 1.0) - 0.5, math.log10(maxY + 1.0) + 0.5)

        rendererX.setSetting(AxisRenderer.LABEL, s"Time ($timeUnit)")
        rendererY.setSetting(AxisRenderer.LABEL, "log2(normalized counts)")

        rendererX.setSetting(AxisRenderer.TICKS_MINOR, false)

        rendererY.setSetting(AxisRenderer.TICKS_MINOR, false)

        // rendererX.setSetting(AxisRenderer.TICKS_SPACING, 2)
        // rendererY.setSetting(AxisRenderer.TICKS_SPACING, 2)
        // rendererX.setSetting( AxisRenderer.TICK_LABELS_FORMAT, choiceFormat );
        // rendererY.setSetting(AxisRenderer.LABEL_DISTANCE, 32)

        val titlefont = new Font(null, Font.PLAIN, 12)

        plotHIV.setSetting(Plot.TITLE, title.grouped(30).mkString("\n") + " : Gene expression pattern");
        plotHIV.setSetting(Plot.TITLE_FONT, titlefont)

        val legend = plotHIV.getLegend()
        legend.clear()
        series.groupBy(_._1).mapValues(_.head).values.foreach {
          case (color, s, name) =>
            s.setName(name)
            legend.add(s)
        }
        plotHIV.setSetting(Plot.LEGEND, true)
        plotHIV.setSetting(Plot.LEGEND_DISTANCE, legendDistance)
        plotHIV.setSetting(Plot.LEGEND_LOCATION, legendLocation)
        // legend.setSetting(Legend.GAP, new Dimension2D.Double(1.0, 1.0))

        // legend.setSetting(Legend.SYMBOL_SIZE, new Dimension2D.Double(0.0, 0.0))

        plotHIV
      } else {
        new XYPlot()
      }
    }

    if (!geneexpressions.isEmpty) {
      val maxY = geneexpressions.flatMap(_.expression.map(_._2)).max
      val minY = geneexpressions.flatMap(_.expression.map(_._2)).min

      val (restingData, activatedData) = geneexpressions.partition(_.activation == Resting)

      val activatedPlot = createPlot(
        activatedData.map { ge =>
          val col = if (ge.infection == Mock) ColorMap(ge.activation).brighter else ColorMap(ge.activation).darker
          (col, ge.expression, (ge.activation.toString + " " + ge.infection))
        }.take(2000), maxY, minY, "Activated", "hours", Location.EAST, 0)

      val restingPlot = createPlot(
        restingData.map { ge =>
          val col = if (ge.infection == Mock) ColorMap(ge.activation).brighter else ColorMap(ge.activation).darker
          (col, ge.expression, (ge.activation.toString + " " + ge.infection))
        }.take(2000),
        maxY, minY, "Resting", "weeks", Location.SOUTH, 3)

      restingPlot.setAxis(XYPlot.AXIS_Y, activatedPlot.getAxis(XYPlot.AXIS_Y))

      val rendererYHIV = activatedPlot.getAxisRenderer(XYPlot.AXIS_Y)
      rendererYHIV.setSetting(AxisRenderer.TICK_LABELS, false)
      rendererYHIV.setSetting(AxisRenderer.SHAPE_VISIBLE, false)
      rendererYHIV.setSetting(AxisRenderer.TICKS, false)
      rendererYHIV.setSetting(AxisRenderer.LABEL, "")
      rendererYHIV.setSetting(AxisRenderer.LABEL_DISTANCE, 0)

      val insetsTop = 20.0
      val insetsLeft = 90.0
      val insetsBottom = 95.0
      val insetsRight = 80.0
      activatedPlot.setInsets(new Insets2D.Double(
        insetsTop, 5, insetsBottom, 110));
      restingPlot.setInsets(new Insets2D.Double(
        insetsTop, insetsLeft, insetsBottom, 20));

      val container = new DrawableContainer(new TableLayout(2));
      container.add(restingPlot);
      container.add(activatedPlot);

      container
    } else {
      new DrawableContainer(new TableLayout(2));
    }

  }

}