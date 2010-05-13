package org.purview.principalcomponentanalyser

import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import scala.math._

sealed class EigenvalueDecomposition(inputMatrix: Matrix[Float]) {
  private val _matrixDimension = inputMatrix.width //aka: N
  private val _realEigenValues = new Array[Float](_matrixDimension) //aka: d
  private val _imaginaryEigenValues = new Array[Float](_matrixDimension) //aka: e
  private val _vectors = new MutableArrayMatrix[Float](_matrixDimension, _matrixDimension) //aka: V

  def vectors: Matrix[Float] = _vectors
  def values: Seq[Float] = _realEigenValues
  init()

  private def init() = {
    var x = 0
    while (x < _matrixDimension) {
      var y = 0
      while (y < _matrixDimension) {
        _vectors(x, y) = inputMatrix(x, y)
        y += 1
      }
      x += 1
    }
    tred2()
    tql2()
  }

  private def hypot(a: Float, b: Float) = if(abs(a) > abs(b)) {
    val tmp = b / a
    abs(a) * sqrt(1 + tmp * tmp).toFloat
  } else if(b != 0) {
    val tmp = a / b
    abs(b) * sqrt(1 + tmp * tmp).toFloat
  } else 0f

  private def tred2(): Unit = {
    var j0 = 0
    while (j0 < _matrixDimension) {
      _realEigenValues(j0) = _vectors(_matrixDimension - 1, j0)
      j0 += 1
    }

    var x = _matrixDimension - 1
    while (x > 0) {
      var scale = 0f
      var h = 0f
      var k = 0
      while (k < x) {
        scale = scale + abs(_realEigenValues(k)).toFloat
        k += 1
      }
      if (scale == 0f) {
        _imaginaryEigenValues(x) = _realEigenValues(x - 1)

        var j1 = 0
        while (j1 < x) {
          _realEigenValues(j1) = _vectors(x - 1, j1)
          _vectors(x, j1) = 0f
          _vectors(j1, x) = 0f
          j1 += 1
        }
      } else {
        var k = 0
        while (k < x) {
          _realEigenValues(k) /= scale
          h += _realEigenValues(k) * _realEigenValues(k)
          k += 1
        }

        var f = _realEigenValues(x - 1)
        var g = sqrt(h).toFloat
        if (f > 0)
          g = -g
        _imaginaryEigenValues(x) = scale * g
        h = h - f * g
        _realEigenValues(x - 1) = f - g

        var j2 = 0
        while (j2 < x) {
          _imaginaryEigenValues(j2) = 0f
          j2 += 1
        }

        j2 = 0
        while (j2 < x) {
          f = _realEigenValues(j2)
          _vectors(j2, x) = f
          g = _imaginaryEigenValues(j2) + _vectors(j2, j2) * f

          var k = j2 + 1
          while (k <= x - 1) {
            g += _vectors(k, j2) * _realEigenValues(k)
            _imaginaryEigenValues(k) += _vectors(k, j2) * f
            k += 1
          }
          _imaginaryEigenValues(j2) = g
          j2 += 1
        }
        f = 0f

        j2 = 0
        while (j2 < x) {
          _imaginaryEigenValues(j2) /= h
          f += _imaginaryEigenValues(j2) * _realEigenValues(j2)
          j2 += 1
        }
        var hh = f / (h + h)
        j2 = 0
        while (j2 < x) {
          _imaginaryEigenValues(j2) -= hh * _realEigenValues(j2)
          j2 += 1
        }
        j2 = 0
        while (j2 < x) {
          f = _realEigenValues(j2)
          g = _imaginaryEigenValues(j2)

          var k: Int = j2
          while (k <= x - 1) {
            _vectors(k, j2) -= (f * _imaginaryEigenValues(k) + g * _realEigenValues(k))
            k += 1
          }
          _realEigenValues(j2) = _vectors(x - 1, j2)
          _vectors(x, j2) = 0f
          j2 += 1
        }
      }
      _realEigenValues(x) = h
      x -= 1
    }

    x = 0
    while (x < _matrixDimension - 1) {
      _vectors(_matrixDimension - 1, x) = _vectors(x, x)
      _vectors(x, x) = 1f
      var h = _realEigenValues(x + 1)
      if (h != 0f) {

        var k = 0
        while (k <= x) {
          _realEigenValues(k) = _vectors(k, x + 1) / h
          k += 1
        }

        var j = 0
        while (j <= x) {
          var g = 0f

          var k = 0
          while (k <= x) {
            g += _vectors(k, x + 1) * _vectors(k, j)
            k += 1
          }

          k = 0
          while (k <= x) {
            _vectors(k, j) -= g * _realEigenValues(k)
            k += 1
          }
          j += 1
        }
      }

      var k = 0
      while (k <= x) {
        _vectors(k, x + 1) = 0f
        k += 1
      }
      x += 1
    }

    j0 = 0
    while (j0 < _matrixDimension) {
      _realEigenValues(j0) = _vectors(_matrixDimension - 1, j0)
      _vectors(_matrixDimension - 1, j0) = 0f
      j0 += 1
    }
    _vectors(_matrixDimension - 1, _matrixDimension - 1) = 1f
    _imaginaryEigenValues(0) = 0f
  }

  private def tql2(): Unit = {
    var i = 1
    while (i < _matrixDimension) {
      _imaginaryEigenValues(i - 1) = _imaginaryEigenValues(i)
      i += 1
    }
    _imaginaryEigenValues(_matrixDimension - 1) = 0f
    var f = 0f
    var tst1 = 0f
    var eps = pow(2.0, -52.0).toFloat

    var l = 0
    while (l < _matrixDimension) {
      tst1 = max(tst1, abs(_realEigenValues(l)) + abs(_imaginaryEigenValues(l)))
      var m = l
      while (m < _matrixDimension && !(abs(_imaginaryEigenValues(m)) <= eps * tst1)) {
        m += 1
      }
      if (m > l) {
        var iter = 0
        do {
          iter = iter + 1
          var g = _realEigenValues(l)
          var p = (_realEigenValues(l + 1) - g) / (2f * _imaginaryEigenValues(l))
          var r = hypot(p, 1)
          if (p < 0) {
            r = -r
          }
          _realEigenValues(l) = _imaginaryEigenValues(l) / (p + r)
          _realEigenValues(l + 1) = _imaginaryEigenValues(l) * (p + r)
          var dl1 = _realEigenValues(l + 1)
          var h = g - _realEigenValues(l)

          var i: Int = l + 2
          while (i < _matrixDimension) {
            _realEigenValues(i) -= h
            i += 1
          }
          f = f + h
          p = _realEigenValues(m)
          var c = 1f
          var c2 = c
          var c3 = c
          var el1 = _imaginaryEigenValues(l + 1)
          var s = 0f
          var s2 = 0f

          i = m - 1
          while (i >= l) {
            c3 = c2
            c2 = c
            s2 = s
            g = c * _imaginaryEigenValues(i)
            h = c * p
            r = hypot(p, _imaginaryEigenValues(i))
            _imaginaryEigenValues(i + 1) = s * r
            s = _imaginaryEigenValues(i) / r
            c = p / r
            p = c * _realEigenValues(i) - s * g
            _realEigenValues(i + 1) = h + s * (c * g + s * _realEigenValues(i))

            var k: Int = 0
            while (k < _matrixDimension) {
              h = _vectors(k, i + 1)
              _vectors(k, i + 1) = s * _vectors(k, i) + c * h
              _vectors(k, i) = c * _vectors(k, i) - s * h
              k += 1
            }
            i -= 1
          }
          p = -s * s2 * c3 * el1 * _imaginaryEigenValues(l) / dl1
          _imaginaryEigenValues(l) = s * p
          _realEigenValues(l) = c * p
        } while (abs(_imaginaryEigenValues(l)) > eps * tst1)
      }
      _realEigenValues(l) = _realEigenValues(l) + f
      _imaginaryEigenValues(l) = 0f
      l += 1
    }

    i = 0
    while (i < _matrixDimension - 1) {
      var k = i
      var p = _realEigenValues(i)

      var j = i + 1
      while (j < _matrixDimension) {
        if (_realEigenValues(j) < p) {
          k = j
          p = _realEigenValues(j)
        }
        j += 1
      }
      if(k != i) {
        _realEigenValues(k) = _realEigenValues(i)
        _realEigenValues(i) = p

        var j = 0
        while (j < _matrixDimension) {
          p = _vectors(j, i)
          _vectors(j, i) = _vectors(j, k)
          _vectors(j, k) = p
          j += 1
        }
      }
      i += 1
    }
  }
}

