package org.purview.core.analysis

import org.purview.core.data.Matrix
import org.purview.core.data.MutableMatrix
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.Point
import org.purview.core.report.ReportEntry
import org.purview.core.stage.Stage
import org.specs.Specification
import org.specs.runner.JUnit4
import scala.util.Random

class AnalyserSpecTest extends JUnit4(AnalyserSpec)

object AnalyserSpec extends Specification {
  def randomNumbers(max: Int): Stream[Int] = Stream.cons(Random.nextInt(max), randomNumbers(max))

  "An analyser" should {
    "be able to generate reports" in {
      val a = new Analyser[Int] {
        def analyse(i: Int) = Set(new ReportEntry with Message {
            val level = Information
            val message = "Analysed " + i
          })
      }
      a.analyse(0) must not be empty
      a.analyse(2).foreach(x => x.asInstanceOf[Message].message must_== "Analysed 2")
    }
  }

  "A heat map analyser" should {
    val matrix = new MutableMatrix[Float](16, 16)

    "output a predefined message" in {
      val msg = "Hello, jakhlk"
      val analyser = new HeatMapAnalyser[Float] {
        val heatmap = Stage.identity[Matrix[Float]]
        override val message = msg
      }
      matrix(0, 0) = 500
      for(entry <- analyser.analyse(matrix)) {
        entry.asInstanceOf[Message].message must_== msg
      }
    }

    "find a rogue maximum in a matrix" in {
      val analyser = new HeatMapAnalyser[Float] {
        val heatmap = Stage.identity[Matrix[Float]]
      }
      val posX = Random.nextInt(16)
      val posY = Random.nextInt(16)
      matrix(posX, posY) = 9000 //high number

      val report = analyser.analyse(matrix)

      report must have size 1 //there's only one rogue, right?

      for(entry <- report) {
        val p = entry.asInstanceOf[Point]
        p.x must_== posX
        p.y must_== posY
      }
    }

    "find maximi of the same magnitude" in {
      val randomPoints = (randomNumbers(16) zip randomNumbers(16)).distinct.take(16).force

      randomPoints.foreach(point => matrix(point._1, point._2) = 9000)

      val analyser = new HeatMapAnalyser[Float] {
        val heatmap = Stage.identity[Matrix[Float]]
        override val maxDistanceForMerge = 0f
      }

      val report = analyser.analyse(matrix)

      report must have size 16

      for(entry <- report) {
        val p = entry.asInstanceOf[Point]
        randomPoints must contain (p.x, p.y)
      }
    }

    "merge nearby maximi in a row" in {
      matrix(1, 0) = 9000
      matrix(3, 0) = 9000
      matrix(5, 0) = 9000
      matrix(15, 0) = 9000

      val analyser = new HeatMapAnalyser[Float] {
        val heatmap = Stage.identity[Matrix[Float]]
        override val maxDistanceForMerge = 6f
      }

      val report = analyser.analyse(matrix)

      report must have size 2
    }

    "merge nearby maximi in a cloud" in {
      matrix(1, 1) = 9000
      matrix(3, 3) = 9000
      matrix(5, 5) = 9000
      matrix(15, 15) = 9000

      val analyser = new HeatMapAnalyser[Float] {
        val heatmap = Stage.identity[Matrix[Float]]
        override val maxDistanceForMerge = 6f
      }

      val report = analyser.analyse(matrix)

      report must have size 2
    }
  }
}