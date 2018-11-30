const _ = require("lodash")
const Q = require("./QueryHelpers")
const { nonArrayChildren } = Q


const normalize = (v) => {
  if (typeof v === "object") {
    return v
  } else {
    try {
      return v.toString()
    } catch (x) {
      return v 
    }
  }
}


const alwaysArray = (obj) => obj instanceof Array ? obj : [obj]


class ObjectPath {
  /*
    parent // ObjectPath?
    parentProperty // string?
    _children // ObjectPath[]
    values // any[]
    _asJs // any
    _parentObj // any
  */

  constructor(obj, parent, parentProperty, parentObj) {
    this.parent = parent
    this.parentProperty = parentProperty
    this._objs = alwaysArray(obj)
    this._parentObj = parentObj

    if (this.parent && undefined === parentProperty) {
      throw new TypeError("Child ObjectPath requires `parentProperty`")
    }
  }

  get cardinalityMany() {
    return this._objs.length !== 1
  }

  get values() {
    return this._objs
  }

  get children() {
    if (!this._children) {
      this._children = _.flatten(
        this._objs.filter(o=>o).map(o=>{
          const entries = Object.entries(o)
          if (entries.length) {
            return entries.map(t=>new ObjectPath(t[1], this, t[0], o))
          } else {
            return []
          }
        })
      )
    }

    return this._children
  }

  // string[] -> ObjectPath[]
  get(path) {
    if (path.length > 0) {
      return _.flatten(
        this.children
          .filter(c=>c.parentProperty === path[0])
          .map(c=>c.get(_.tail(path)))
      ) 

    } else {  
      return [this]
    }
  }

  
  // string[] -> any[]
  flattenTo(path, delimeter) {
    delimeter = delimeter || "."
    
    if (path.length === 1) {
      return this._objs 
    }

    const walkParents = (path, op) => {
      if (path.length === 1) {
        const tmp = {... op._parentObj}
        delete tmp[path]
        return nonArrayChildren(tmp)
      }

      const obj = nonArrayChildren(op._parentObj)
      const prefix = _.initial(path).join(delimeter)
      const tmp = Object.assign({}, 
        _.mapKeys(obj, (v, k) => prefix + delimeter + k),
        walkParents(_.initial(path), op.parent)
      )
      
      return tmp
    }

    const ops = this.get(path)
    const prefix = _.initial(path).join(delimeter)
    return ops.map(op=>walkParents(path,op))
  }

  getValues(path) {
    return _.flatten(this.get(path).map(op=>op.values))
  }

  getParents(path) {
    if (path.length === 0) 
      throw new TypeError("Root element does not have parent")
    return this.get(_.initial(path))
  }

  filter(pred) {
    return this.children.filter(pred)
  }

  filterChildren(path, pred) {
    return this.get(path).filter(pred)
  }
 
  copy(nextValues /*any[]*/) {
    return new ObjectPath(
      nextValues,
      this.parent,
      this.parentProperty
    )
  }
  
  
  unsafe_setValues(nextValues) {
    const parentObj = this._parentObj
    this.parent._objs = [].concat(
      this.parent._objs.filter(o=>parentObj !== o),
      nextValues
    )
    this.parent._children = undefined
    
    if (this.parent.parent) {
      for (const o of this.parent.parent._objs) {
        const p = this.parent.parentProperty
        if (o[p] === parentObj) {
          o[p] = this.parent.cardinalityMany ? nextValues : nextValues[0]
        }
      }
  
      this.parent.parent._children = undefined
    }
  }

  toJs() {
    this._asJs = this.cardinalityMany ? this._objs : this._objs[0]
   
    return this._asJs
  }

  // To call following unsafe mutation
  /*
  resyncValues() {
    if (!this.children.length) return 

    this._objs = this.children.map(c=>{
      c.resyncValues()
      const res = {}
      for (const c2 of c.children) {
        res[c2.parentProperty] = c2.values
      }
      return res
    })
  }*/
}


// Copies objects
const noDupeKeysAssign = (primary, secondary) => {
  const res = {... primary}
  for (const t of Object.entries(secondary)) {
    let key = t[0]
    const value = t[1]
    if (undefined !== res[key]) {
      for (let i = 1; true; i++) {
        const nextKey = key + `_${i}`
        if (undefined === res[nextKey]) {
          key = nextKey
          break
        }
      }
    }

    res[key] = value
  }

  return res 
}

// Copies objects
const mergeAll = (arrA, arrB) => {
  /*console.log("\nmergeAll:")
  console.log(JSON.stringify(arrA))
  console.log(JSON.stringify(arrB))
  console.log("\n")*/
  return _.flatten(arrA.map(vA=> 
    arrB.map(vB=>noDupeKeysAssign(vA, vB))
  ))
}


class ObjectPathRoot extends ObjectPath {
  constructor(obj) {
    super(obj)
  }

  copyDeep() {
    const toClone = this.cardinalityMany ? this._objs : this._objs[0]
    return new ObjectPathRoot(JSON.parse(JSON.stringify(toClone)))
  }

  leftJoin(joinLevelPath /*string[]*/, right0 /*ObjectPathRoot*/, leftPath, rightPath) {
    const compare = (left /*any[]*/)  => {
      const leftN = left.map(normalize)
      return (right) => {
        return _.intersection(leftN, right.map(normalize)).length > 0
      }
    }

    const leftRoot = this.copyDeep()
    const rightRoot = right0.copyDeep()
    
    for (const leftParent of leftRoot.getParents(joinLevelPath)) {
      for (const leftChild of leftParent.get(leftPath)) {
        const comparer = compare(leftChild.values)
        let accum = []
        for (const rightChild of rightRoot.get(rightPath)) {
          if (comparer(rightChild.values)) {
            accum.push(noDupeKeysAssign(leftChild._parentObj, rightChild._parentObj))  
            
          } else {
            // pass
          }
        }

        if (accum.length) {
          // A hack - we unsafely update parent to avoid walking and 
          // rebuilding entire object tree.
          // FIXME: This leves ._objs out of sync
          leftChild.unsafe_setValues(accum)
        }
      }
    }

    return leftRoot
  }
}

//export default ObjectPathRoot
module.exports = ObjectPathRoot


