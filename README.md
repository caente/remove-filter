# remove-filter

Removes a field from a given case class, e.g.

```
sealed trait Animal
case class Cat(name: String, fish: Int) extends Animal
case class Dog(name: String, bones: Int) extends Animal
val felix:Cat = Cat("Felix", 1)
assert( felix.removeField('name) == 1 :: HNil)
```

this turned out to be impossible, so I wrote this instead https://github.com/caente/equals-except since that was what I needed...
