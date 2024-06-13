 - you can create a template with the name "elements", but you can't call it

 ```
 template withElems {
	elements(1)
	elements(2)
	
}

template callElems(elements: Int) {
	withElems {
		output(elements)
		elements
	}
}

template elements(a: Int) {
	output(a)
}

page root {
	withElems
}
```

Pomysły : make handlers class calls to avoid fixing them in denotation, like with lift

- How to make handlers that execute IO?

How does this work?
Now, Every property is evaluated and given a location.
What I would want to happen, is first, refer each var to a Box
then if that name appears, it is not evaluated above passing a box
However
then I want to evaluate the objects themselves
and in the database I would want to update the boxes to their uuids

1. I evaluate the names. Each name points to an empty value
2. I evaluate the objects
should entities have their own environments?
Yes, because you need old and new of these
so its an entity / null environment
So entities are actually passed around as pointers to the entity environment. Funny.
so its env -> store -> env kind of deal, yet again. 
and when all the objects are evaluated, I store them in the db. BUT with the caveat that instead of boxes, entities are now referred to as uuids. This translation happens when accessing the database. 
so the key is actually : location, Maybe (uuid), and value is Maybe (EDef).
So just location is enough to identify it, but if the object exists it is also indexed by uuid and you can find that. This is doable.

- ask if reading should only happen once per rendering??