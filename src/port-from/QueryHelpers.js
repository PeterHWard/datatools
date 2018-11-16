const _ = require("lodash")

const matchAny = (keys, obj_1, obj_2) => {
  for (let k of keys) {
    let v1 = obj_1[k]
    let v2 = obj_2[k] 
    if (undefined !== v1 && v1 === v2) 
      return true
  }

  return false
}


const filterByPredicate = (arr, pred) => {
  return arr.filter(o=>pred(o))
}


const filterByIds = (arr, ids) => {
  return filterByPredicate(arr, (o=>ids.includes(o)))
}


const arrayToMap = (arr, prop) => {
  if (prop) {
    if (/\./.test(prop)) {
      // NB: Don't use getNestedValue on `arr`
      const keys = arr.map(o=> getNestedValue(o, prop, false))
      return _.zipObject(keys, arr)
    } else {
      return _.zipObject(arr.map(o=>o[prop]), arr)
    }
  }
  else return _.zipObject(arr, arr)
}


const assocReduce = (col_1, col_2, op) => {
  let remaining_2 = [... col_2]
  let reduced = []
  let abandoned_1 = []
  for (let item_1 of col_1) {
    let res;
    for (let i = 0; i < remaining_2.length; i++) {
      let item_2 = remaining_2[i]
      res = op(item_1, item_2)
      if (res) {
        remaining_2.splice(i, 1)
        break;
      }
    }

    if (res) reduced.push(res)
    else abandoned_1.push(item_1)
  }
  
  let abandoned_2 = remaining_2
  return {
    reduced,
    abandoned_1,
    abandoned_2,
    
    concat: () => {
      return [].concat(
        reduced,
        abandoned_1,
        abandoned_2
      )
    }
  }

}


const keysMatch = (keys, obj_1, obj_2) => {
  for (let k of keys) {
    if (obj_1[k] !== obj_2[k]) return false
  }

  return true
}


const upsert = (db, keyProps, updates) => {
  return assocReduce(db, updates, (existing, update)=> {
    if (keysMatch(keyProps, existing, update)) {
      return Object.assign(existing, update)
    }
  }).concat()
}

const getNestedParent = (db, propsStr, expectArrays) => {
  const props = _.initial(propsStr.split("."))
  if (props.length > 0) return getNestedValue(db, props, expectArrays)
  else {
    if (expectArrays) 
      return ((db instanceof Array) ? db :[db]).filter(o=>{
        return (o !== undefined) && (o[propsStr] !== undefined)
      })
    else 
      return db
  }
}


// Rather than assume table, recursively deals with arrays at any level
const getNestedValue = (db, propsStr, expectArrays) => {
  if (db === undefined) return expectArrays ? [] : undefined
  
  if (!(db instanceof Array)) {
    db = [db]
  }

  const props = (propsStr instanceof Array) 
    ? propsStr
    : propsStr.split(".").filter(p=>p.trim().length > 0)
  
  if (!props.length) {
    throw new Error("Props path must include at least on property")
  }

  const values = _.flatten(db.map(dbItem => {
    const value = dbItem[_.head(props)]
    const tail = _.tail(props)
    const tmp = tail.length 
      ? getNestedValue(value, tail, true)
      : [value]

    return tmp
  }))
  
  if (expectArrays) {
    return _.flatten(values)
  } else {
    if (values.length > 1)
      throw new Error("Unexpected array encountered")

    return values[0]
  }
}


const alwaysArray = (obj) => {
  if (undefined === obj) return []
  return (obj instanceof Array) ? obj : [obj]
}

/*
const flattenToProp = (data, path, delimiter) => {
  delimiter = delimiter || "."
  const inner = (ds, prop) => {
    console.log(ds, prop)
    return _.flatten(alwaysArray(ds).map(row=>alwaysArray(row[prop]).map(o=>{
      row = {... row}
      delete row[prop]

      return Object.assign(
        row, 
        _.mapKeys(nonArrayChildren(o), (v, k) => prop + delimiter + k))
    })))
  }
  
  const allProps = _.initial(path)
  let prefix = ""
  for (const prop of allProps) {
    prefix = prefix ? prefix + delimiter + prop : prop
    data = inner(data, prefix)
    //prefix = prefix ? prefix + "." + prop : prop
    console.log(prefix, data)
  }

  //console.log(data[0])
  return data
}*/

// Always returns Array. Empty Array if property never found.
const deepLiftAll = (data, path) => {
  const pathStr = path.join(".")
  return _.flatten(
    _.flatten(flattenToProp(data, path)).map(o=>o[pathStr] || []))
}

const deepEachAll = (data, path, each) => {
  const deepEach = (parent, subPath) => {
    if (subPath.left === 0) {
      each(parent)
    } else {
      let arr
      if (parent !== undefined) arr = alwaysArray(parent)
      // Create child object for parent
      else {
        child = {}
        parent[_.first(subPath)] = child
        arr = [child]
      }
    }
    arr.map(o=>deepEach(o, _.tail(subPath)))
  }
  alwaysArray(data).map(deepSetAux(o, path))
}
// If data undefined or empty Array no setting takes place.
// However child objects will automatically be created for 
// sub-paths if missing: {}, ["foo", "bar"], 42 -> {foo: {bar: 42}
const deepSetAll = (data, path, update) => {
  const deepSetAux = (parent, subPath) => {
    if (subPath.left === 0) {
      throw new TypeError("path must contain at least on property")
    } else if (subPath.length === 1) {
      parent[subPath] = update
    } else {
      let arr
      if (parent !== undefined) arr = alwaysArray(parent)
      // Create child object for parent
      else {
        child = {}
        parent[_.first(subPath)] = child
        arr = [child]
      }
    }
    arr.map(o=>deepSetAux(o, _.tail(subPath)))
  }
  alwaysArray(data).map(deepSetAux(o, path))
}

const nonArrayChildren = (obj) => {
  const isBase =(o) => {
    return !o 
      || (o instanceof Array) 
      || (o.length) 
      || Object.keys(o).length === 0
  }
  
  if (isBase(obj)) return obj

  const res = {}
  for (const t of Object.entries(obj)) {
    Object.assign(
      res, 
      isBase(t[1]) 
        ? {[t[0]]: t[1]} 
        : _.mapKeys(nonArrayChildren(t[1]), (v, k) => t[0] + "." + k))
  }

  return res
}

// Shallow equal if `compare` undefined
const areEqual = (arr_1, arr_2, compare) => {
  if (arr_1.length != arr_2.length) return false
  for (let i = 0; i < arr_1.length; i++) {
    if (arr_1[i] !== arr_2[i]) return false
  }

  return true
}


const haveIntersection = (arr_1, arr_2) => {
  for (let v of arr_1) {
    if (arr_2.includes(v)) return true
  }

  return false 
}


const isDeepNonEmpty = (arr) => {
  const inner = (maybeArr) => {
    if (maybeArr instanceof Array) {
      return maybeArr.filter(v=>inner(v)).length > 0
    } else {
      return undefined !== maybeArr
    }
  }

  return inner(arr)
}


class FilterMemo {
  // Expects type {[String]: any[]}
  constructor(dataSource) {
    this.dataSource = dataSource
  }

  // Returns array of items matching pred.
  // Updates internal store with items not passing pred.
  filter(key, pred) {
    const arr = this.dataSource[key] || []
    const retVal = arr.filter(pred)
    this.dataSource[key] = arr.filter((o)=>!retVal.includes(o))
    return retVal
  } 
}

const isSubPathOf = (pathA, pathB) => {
  if (pathA.length > pathB.length) return false
  for (let i = 0; i < pathA.length; i++) {
    if (pathA[i] !== pathB[i]) return false
  }
  
  return true
}

const isSubPathOrPeerOf = (pathA, pathB) => {
  if (pathA.length > pathB.length) return false
  for (let i = 0; i < pathA.length - 1; i++) {
    if (pathA[i] !== pathB[i]) return false
  }
  
  return true
}

// objs any[], pathKey: string 
const toTree = (objs, pathKey) => {
  const res = []
  for (const o of objs) {
    const path = o[pathKey].split(".")
    let children = res 
    for (let i = 0; i < path.length; i++) {
      const prop = path[i]
      let parent = children.filter(c=>c.key === prop)[0]
      if (!parent) {
        parent = {
          key: prop, 
          children: []
        }
        children.push(parent)
      }
      if (i === path.length - 1) parent.data = o 
      children = parent.children
    }
  }

  return res
}

// All left objects included at least once. Only right 
// objects that can be joined included.
// User string[] for path
const leftJoin = (leftObjs, rightObjs, leftPath, rightPath) => {
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

  const compare = (datumA, rightProp) => {
    datumAStr = normalize(datumA)
    return (objB) => datumAStr === normalize(objB[rightProp])
  }
  
  const leftParents = getNestedParent(leftObjs, leftPath.join("."))
  const rightParents = getNestedParent(rightObjs, rightPath.join("."))
  
  _.flatten(leftParents.map(leftP=>{
    const comparer = compare(leftP[_.last(leftPath)], _.last(rightPath))
    const joined = rightParents.filter(comparer).map(rightP=>{
      return Object.assign({}, leftP, rightP)
    })

    return (joined.length) ? joined : [leftP]
  }))

  if (leftPath.length > 1) {

  } else {

  }
}
/*
const leftJoin = (leftObjs, rightObjs, leftPath, rightPath) => {
  const compare = (a, b) => {
    if (a === null || b === null) return false
    return a === b 
  }

  // For comparison
  const normalize = (v) => {
    try {
      return v.toString()
    } catch (x) {
      return null
    }
  }
  
  const groupValues = (objs, path) => {
    return _.groupBy(
      _.flatten(objs.map(o=>deepLiftAll(o, path).map(v=>[normalize(v), o]))),
      t=>t[0])
  }
  const groupedA = groupValues(leftObjs, leftPath)
  const groupedB = groupValues(rightObjs, rightPath)
  
  return _.flatten(groupedA.map(a=>({
    left: a, 
    rights: groupedB.filter(b=>compare(b[0], a[0]))
  })).map(lr=>{
    if (lr.rights.length === 0) {
      return lr.left[1]
    } else {
      return _.flatten(lr.left[1].map(left=>{
        return lr.rights.map(right=>deepEachAll(
          {... left}, 
          _.initial(leftPath),
          o=>Object.assign(o, right)
        ))
      }))
    }
  }))
}*/


const Q = {
  filterByPredicate,
  filterByIds,
  assocReduce,
  arrayToMap,
  getNestedValue,
  getNestedParent,
  upsert,
  matchAny,
  areEqual,
  haveIntersection,
  isDeepNonEmpty,
  FilterMemo,
  isSubPathOf,
  isSubPathOrPeerOf,
  alwaysArray,
  nonArrayChildren,
  toTree,
  leftJoin
}

module.exports = Q


const test_assocReduce = () => {
  let res = assocReduce(
    [
      {id: 1, value: "1a"},
      {id: 2, value: "2a"},
      {id: 3, value: "3a"}, 
    ], 
    [
      {id: 2, value: "2b"},
      {id: 3, value: "3b"}, 
      {id: 4, value: "4b"}, 
    ], 
    (a, b) => {
      if (a.id == b.id) return {... b}
    }
  )

  /*
  console.log(res)
  console.log(res.concat())
  console.assert(res.reduced.length === 2)  
  console.assert(res.abandoned_1.length === 1) 
  console.assert(res.abandoned_2.length === 1)  
  console.assert(res.abandoned_1[0].id !== 1)  
  console.assert(res.abandoned_2[0].id !== 4)
  console.assert(res.concat().length === 4)
  */
  let map = Q.arrayToMap(res.concat(), "id")
  assert.deepEqual(map[1].value, "1a")
  assert.deepEqual(map[2].value, "2b")
  assert.deepEqual(map[3].value, "3b")
  assert.deepEqual(map[4].value, "4b")
}

const test_getNestedValue = (assert) => {
  let res_1 = getNestedValue({foo: "bar"}, "foo")
  assert.deepEqual("bar", res_1)
  console.log("after 1")
  
  let res_2 = getNestedValue([{foo: "bar"}], "foo", true)
  assert.deepEqual(["bar"], res_2)
  console.log("after 2")
  
  let res_3 = getNestedValue([{foo: ["bar"]}], "foo", true)
  assert.deepEqual([["bar"]], res_3)
  console.log("after 3")
  
  let res_4 = getNestedValue(
    { 
      foo: {baz: "bar"} 
    }, 
    "foo.baz")
  assert.deepEqual("bar", res_4)

  let res_5 = getNestedValue(
    { 
      foo: [{baz: "bar"}]
    }, 
    "foo.baz", 
    true
  )
  assert.deepEqual(["bar"], res_5)

  let res_6 = getNestedValue(
    { 
      foo: [{baz: "bar"}]
    }, 
    ["foo", "baz"], 
    true
  )
  assert.deepEqual(["bar"], res_6)

  let res_7 = getNestedValue(
    [
      {foo: 
        [ {baz: "bar"} ]
      }
    ],
    ["foo", "baz"], 
    true
  )
  assert.deepEqual(["bar"], res_7)
}

/*
if (process.env.NODE_ENV == "development") {
  window.test_QueryHelpers = () => {
    test_assocReduce(assert)
    test_getNestedValue(assert)

    console.log("Tests passed!")
  }
}*/

 