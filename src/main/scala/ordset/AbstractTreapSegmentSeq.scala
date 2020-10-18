package ordset

import ordset.domain.Domain

/**
 * {{{
 *                                __________________________  *
 *                             ↙                                  ↘
 *                __________  * _________________                   \
 *             ↙                                   ↘                 *
 *           *  _                                    \
 *                ↘                       __________ *
 *                 \                   ↙
 *                  \                *  ___
 *                   *                      ↘
 *                                           *
 *
 *       0   |   1   |   2   |   3   |   4   |   5   |   6   |   7   |
 *    -------|-------|-------|-------|-------|-------|-------|-------|-------
 *
 * }}}
 */
abstract class AbstractTreapSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>
}
