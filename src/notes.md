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