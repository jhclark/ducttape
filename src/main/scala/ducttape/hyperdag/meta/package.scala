// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag

/**
 * A layered HyperDAG that allows for multiple hyperedges to atomically participate
 * in a single branch.
 * 
 * Roughly speaking, the correspondence between MetaHyperDAG components and the
 * ducttape components is:
 * A branch point is a MetaEdge
 * Each branch corresponds to a HyperEdge feeding a MetaEdge
 * Each task corresponds to a vertex
 * A realization corresponds to a hyperpath through the MetaHyperDAG
 */
package object meta {}