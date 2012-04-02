package ducttape

/**
 * The low-level formal HyperDAG framework underlying the temporal dependencies
 * among ducttape tasks. The more powerful MetaHyperDag framework in [[ducttape.hyperdag.meta]]
 * is necessary to fully support all ducttape constructs. See that package for more information
 * on mappings between the formal HyperDAG framework and the ducttape domain.
 * 
 * All data structures are immutable and should be constructed using the appropriate Builder
 * classes.
 * 
 * HyperDAGs are a packed data structure meaning there are many different hyperpaths, each of
 * which corresponds to a particular hyperpath through the entire structure. Traversals of
 * unpacked structures are accomplished via an ordered traveral over the unpacked vertices.
 * Each of these unpacked vertices intentionally does not have a backpointer to its parent
 * unpacked vertices so that the entire unpacked structure need not be kept in memory. However,
 * each unpacked vertex does include information about the hyperpath state (realization) of
 * each of its parents.
 * 
 * Some additional variations are also included such as "phantom" vertices, which are never
 * traversed, but may be the source of hyperedges. In ducttape, phantom edges are used so that
 * parameters may introduce hyperedges (branches) without inducing temporal dependencies on
 * other tasks via the HyperDAG.
 * 
 * Asynchronous traversal over both packed and unpacked structures is handled via "walkers"
 * which support asynchronous foreach iteration. See [[ducttape.hyperdag.walker]].
 */
package object hyperdag {}