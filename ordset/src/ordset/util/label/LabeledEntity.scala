package ordset.util.label

/**
 * Base trait for abstract entities which are identified by labels like order, etc.
 * 
 * Labels may be used for equality checks and string representation of entities.
 */
trait LabeledEntity {

  /**
   * Set of labels.
   */
  def labels: Set[Label]
}