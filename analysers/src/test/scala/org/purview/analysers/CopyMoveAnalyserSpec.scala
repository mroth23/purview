package org.purview.analysers

import org.purview.core.data.ImmutableMatrix
import org.specs.SpecificationWithJUnit

class CopyMoveAnalyserSpec extends SpecificationWithJUnit {
  "The CopyMove analyser" should {
    "perform a valid DCT transformation" in {
      val input = new ImmutableMatrix[Float](16, 16, Array(
          - 1, - 1, + 2, + 3, + 5, + 6, + 7, + 5, + 4, + 3, + 3, + 3, + 2, + 2, + 1, + 1,
          + 4, + 4, + 3, + 5, + 6, + 8, + 9, + 9, + 7, + 6, + 7, + 6, + 6, + 6, + 5, + 3,
          + 6, + 5, + 6, + 6, + 6, + 8, + 9, + 9, + 8, + 7, + 9, + 7, + 7, + 8, + 8, + 6,
          + 7, + 8, + 7, + 7, + 6, + 6, + 9, +10, +10, +10, +10, +10, + 9, +10, +10, + 8,
          + 8, + 8, + 8, + 7, + 5, + 6, + 9, +10, +11, +10, +10, +12, +11, +12, +11, +10,
          + 7, + 8, +11, +10, +10, + 8, +11, +11, +10, +10, +11, +11, +10, + 9, +11, + 8,
          + 8, +10, +11, +11, +12, +12, +12, +12, +10, + 9, +10, +11, +11, +10, +10, + 6,
          + 9, + 9, +10, +11, +11, +14, +12, +11, +12, +12, +12, +12, +12, +11, +10, + 8,
          + 7, + 7, + 8, + 8, +10, +13, +11, + 9, +10, +12, +13, +12, +11, +10, +10, + 9,
          + 5, + 6, + 5, + 8, +10, + 7, + 8, + 8, +10, + 9, + 9, +11, + 9, + 9, + 8, + 6,
          + 2, + 4, + 5, + 7, + 8, + 7, + 6, + 8, + 8, + 7, + 8, +10, + 8, + 7, + 7, + 7,
          - 3, - 1, + 0, + 1, + 1, + 1, + 3, + 3, + 3, + 4, + 5, + 6, + 5, + 4, + 2, + 3,
          - 6, - 4, - 4, - 4, - 5, - 5, - 3, - 1, - 4, - 2, + 2, + 3, + 4, + 2, - 1, + 0,
          - 6, - 6, - 8, - 6, - 8, - 5, - 4, - 3, - 6, - 2, - 1, - 1, - 3, - 4, - 6, - 6,
          - 5, - 7, - 7, - 8, - 7, - 7, - 6, - 6, - 5, - 3, - 5, - 6, - 8, - 6, - 6, - 5,
          - 6, - 7, - 7, -10, -10, -12, -11, -12, - 8, - 5, - 5, - 8, - 8, - 6, - 5, - 6))
      val copymove = new CopyMoveAnalyser
      val result = copymove.partialDCT(input, 16)

      val expected = new ImmutableMatrix[Float](16, 16, Array(
          +71.5f    , -12.92271f, -11.54514f, +3.331879f, -3.998945f, +1.815931f, -0.6693439f, -1.558347f, -1.75f, +2.450904f, -0.7510433f, +0.1254027f, -0.5083669f, -0.3896854f, +0.5100575f, -0.0389848f,
          +66.55169f, +5.506096f, -4.925549f, -5.51467f, +0.891021f, +3.064031f, -2.302032f, -0.23269f, -0.2486146f, +1.100402f, -0.9277151f, +1.413502f, +0.2474644f, +0.06914299f, -0.6556489f, +1.026636f,
          -70.26276f, -0.2228446f, +2.291936f, +1.593834f, +5.100437f, -0.3628287f, +1.018435f, +0.1753813f, -1.174112f, +1.208219f, +1.883036f, -1.204654f, +0.7601818f, -0.1344937f, +0.5975081f, -0.04824263f,
          +4.21928f , -2.42056f, -6.736901f, -4.192221f, -4.301154f, +1.579383f, +1.40572f, -0.8148771f, +2.390229f, -0.9225187f, -0.3502898f, -1.179433f, +0.4259266f, -1.014209f, -0.4952169f, -0.7563449f,
          -3.692125f, +7.784853f, -1.882703f, -3.337361f, -0.4312164f, -0.6961951f, +3.273225f, +3.87989f, -1.844359f, -1.382362f, +0.08396991f, +1.587687f, +0.5107231f, +0.5645618f, +0.2142442f, +1.017575f,
          -8.244703f, +1.652285f, -3.406857f, -2.04662f, -3.556088f, +2.793427f, -0.4466516f, -3.162521f, +0.1657905f, +0.2212942f, +0.9858608f, -0.8427125f, +0.3372151f, +1.07621f, -0.7492089f, -0.9944606f,
          -2.304821f, +0.6083202f, +1.7136f, -1.734194f, -0.4458269f, -3.295012f, -2.112726f, -0.002591103f, +1.436623f, -0.1381378f, -1.116783f, +0.4637167f, -0.9417334f, +0.1001457f, +1.923811f, +1.695274f,
          -3.727369f, -1.310456f, -1.417042f, +1.064652f, +0.9479308f, +0.1058384f, +0.8988939f, -0.2578581f, +0.8574969f, +0.1372815f, +0.4373456f, +0.2837379f, -0.7077156f, -0.698072f, +0.9653037f, -0.9598271f,
          -2.874995f, -5.064101f, +2.562944f, +2.059466f, -0.107277f, -0.641979f, -0.05092232f, -0.56397f, -1.125f, -1.449014f, +1.524053f, +0.2260954f, +0.7209308f, -0.3826091f, +0.2085172f, +0.4595101f,
          +2.036416f, +3.211578f, -2.331802f, -1.82209f, +1.112181f, -0.4525491f, -0.04056981f, +0.5970684f, +0.7064189f, -0.7336587f, -0.2020075f, +0.856589f, +0.003774319f, -0.5733041f, +0.4161826f, -1.199295f,
          -4.282182f, +0.2187266f, +2.035897f, +0.5416111f, -2.211579f, +0.09201268f, -1.412279f, -0.279871f, +0.3714729f, +0.7778767f, -1.002313f, -1.036759f, -0.798948f, +0.2002002f, -0.01755477f, +0.607897f,
          -1.892636f, +1.210747f, -1.325929f, -2.122856f, +0.4230433f, -1.586921f, +1.005026f, -0.1470664f, -0.8735263f, +0.154844f, +0.393141f, +0.2412771f, +0.04770107f, +0.8068112f, +1.209257f, +1.087445f,
          -0.09426145f, -0.08846147f, -0.3451366f, -0.3202442f, -0.8642764f, -1.333522f, +1.311272f, -0.8453206f, -0.85963f, +0.3533345f, +0.5885081f, +0.742865f, +0.8062182f, +0.8808715f, -0.7096272f, -0.05577289f,
          -0.5009752f, -0.6645724f, +1.552912f, -0.7155244f, +0.3127602f, -0.3510194f, -1.008764f, +0.3848386f, +1.099775f, -0.1399364f, +0.1641017f, +0.3559592f, +0.7897035f, -0.1709106f, +0.6489524f, +0.7123198f,
          -2.602582f, +0.1783366f, -0.6979867f, +0.6725325f, +0.1962244f, +0.9421257f, +0.09811896f, +0.3970568f, -0.3719278f, -0.04544737f, +0.5587262f, +0.5523024f, +0.06448358f, -0.8873773f, +0.07310168f, -0.2685034f,
          +0.8878399f, -0.9706849f, +1.555656f, -1.014437f, -0.1566203f, +0.7446259f, +0.353443f, -0.812466f, -0.2305061f, -0.5970545f, -0.365656f, +0.03086993f, +1.231023f, +0.6415788f, -0.216463f, -0.186153f))
      result zip expected foreach { (cell) =>
        //Max 50% difference
        cell._1 / cell._2 must be lessThan 1.5f
        cell._2 / cell._1 must be lessThan 1.5f
      }
    }
  }
}