import shapeless._
object removeField {

  import labelled._
  import ops.record._

  trait RemoveField[T] {
    def removeField(t: T, other:Symbol): HList
  }

  object RemoveField {
    
    implicit def generic[F, G](implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[RemoveField[G]]): RemoveField[F] =
      new RemoveField[F] {
        def removeField(f: F, other:Symbol) = sg.value.removeField(gen.to(f), other)
      }

    implicit def product: RemoveField[HNil] =
      new RemoveField[HNil] {
        def removeField(p: HNil, other:Symbol): HList = HNil
      }

    implicit def product[K <: Symbol, V, T <: HList]
      (implicit
        key: Witness.Aux[K],
        selector: Selector.Aux[FieldType[K, V] :: T, K, Symbol],
        sv: Lazy[RemoveField[V]],
        st: Lazy[RemoveField[T]]
      ): RemoveField[FieldType[K, V] :: T] =
        new RemoveField[FieldType[K, V] :: T] {
          def removeField(p: FieldType[K, V] :: T, other:Symbol): HList = {
        if (selector(p) == other) sv.value.removeField(p.head, other)
        else p.head :: st.value.removeField(p.tail, other)
          }
        }

    implicit def cnil: RemoveField[CNil] =
      new RemoveField[CNil] {
        def removeField(p: CNil, other:Symbol) = HNil
      }

    implicit def coproduct[K <: Symbol, V, T <: Coproduct]
      (implicit
        key: Witness.Aux[K],
        sv: Lazy[RemoveField[V]],
        st: Lazy[RemoveField[T]]
      ): RemoveField[FieldType[K, V] :+: T] =
        new RemoveField[FieldType[K, V] :+: T] {
          def removeField(c: FieldType[K, V] :+: T, other:Symbol): HList =
            c match {
              case Inl( head ) => sv.value.removeField(head, other )
              case Inr( tail ) => st.value.removeField(tail, other )
            }
        }
  }

  implicit class RemoveFieldOps[T](x: T)(implicit removeFieldT: RemoveField[T]) {
    def removeField(other:Symbol): HList = removeFieldT.removeField(x, other)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix:Cat = Cat("Felix", 1)
  val tigger = Dog("Tigger", 2)
}

