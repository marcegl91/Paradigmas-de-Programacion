// Ejercicio 1

let nil = {
  null: true
};

nil.cons = function (o) {
  let aux = Object.create(nil);
  aux.head = o;
  aux.tail = this;
  aux.null = false;

  return aux;
};

let lista12 = nil.cons(2).cons(1);
let lista1 = nil.cons(1);
let listaNil = nil;
let listaNilNil = nil.cons(nil);
let lista11 = nil.cons(1).cons(1);

// Ejercicio 2

nil.toString = function () {
  if (this.null) {
    return '[]';
  } else {
    return '[' + this.head + ':' + this.tail.toString() + ']';
  }
};

console.log("Ejercicio 2 - [1:[2:[]]] == " + lista12.toString());
console.log("Ejercicio 2 - [1:[]] == " + lista1.toString());
console.log("Ejercicio 2 - [] == " + listaNil.toString());
console.log("Ejercicio 2 - [[]:[]] == " + listaNilNil.toString());
console.log("Ejercicio 2 - [1:[1:[]]] == " + lista11.toString());

// Ejercicio 3

nil.length = function () {
  // console.log("Llamado a nil.length");

  if (this.null) {
    return 0;
  } else {
    return 1 + this.tail.length();
  }
};

console.log();
console.log("Ejercicio 3 - " + nil.toString() + ' = ' + nil.length());
console.log("Ejercicio 3 - " + lista12.tail.toString() + ' = ' + lista12.tail.length());
console.log("Ejercicio 3 - " + lista12.toString() + ' = ' + lista12.length());
console.log("Ejercicio 3 - " + lista1.toString() + ' = ' + lista1.length());
console.log("Ejercicio 3 - " + listaNil.toString() + ' = ' + listaNil.length());
console.log("Ejercicio 3 - " + listaNilNil.toString() + ' = ' + listaNilNil.length());
console.log("Ejercicio 3 - " + lista11.toString() + ' = ' + lista11.length());

// Ejercicio 4

nil.copy = function () {
  if (this.null) {
    return Object.create(this);
  } else {
    let copia = Object.create(Object.getPrototypeOf(this));

    copia.null = false;
    copia.head = this.head;
    copia.tail = this.tail.copy();

    return copia;
  }
};

console.log();
console.log("Ejercicio 4: " + nil.copy().toString());
console.log("Ejercicio 4: " + (nil.copy() == nil).toString());
console.log("Ejercicio 4: " + lista12.copy().toString());
console.log("Ejercicio 4: " + (lista12.copy() == lista12).toString());
console.log("Ejercicio 4: " + (lista12.copy().tail == lista12.tail).toString());

// Ejercicio 5

listaMasLarga = Object.create(nil);
listaMasLarga.size = 0;

listaMasLarga.cons = function (o) {
  let aux = Object.create(listaMasLarga);
  aux.head = o;
  aux.tail = this;
  aux.null = false;

  return aux;
};

listaMasLarga.length = function () {
  // console.log("Llamado a listaMasLarga.length");

  if (this.null) {
    return 0;
  } else {
    if (this.size == 0) {
      this.size = 1 + this.tail.length();
    }
    return this.size;
  }
};

let listaMuyLarga = listaMasLarga.cons(4).cons(3).cons(2).cons(1);
let listaMuyCorta = listaMasLarga;
let singleton = listaMasLarga.cons(4);

console.log();
console.log("Ejercicio 5: " + listaMuyLarga.length());
console.log("Ejercicio 5: " + listaMuyLarga.length());
console.log("Ejercicio 5: " + listaMuyLarga.tail.length());
console.log("Ejercicio 5: " + listaMuyCorta.length());
console.log("Ejercicio 5: " + singleton.length());

// Ejercicio 6

nil.concatenar = function (l) {
  let aux;

  if (this.null) {
    aux = Object.create(this);

    if (!l.null) {
      aux.null = false;
      aux.head = l.head;
      aux.tail = this.concatenar(l.tail);
    }
  } else {
    aux = Object.create(Object.getPrototypeOf(this));

    aux.null = false;
    aux.head = this.head;
    aux.tail = this.tail.concatenar(l);
  }

  return aux;
};

let lista34 = nil.cons(4).cons(3);
let listaConcat = lista12.concatenar(lista34);
let listaConcat2 = lista12.concatenar(lista12);
let listaConcat3 = nil.concatenar(nil);
let listaConcat5 = nil.concatenar(lista12);
let listaConcat4 = lista12.concatenar(nil);

console.log();
console.log("Ejercicio 6: " + lista34.toString());
console.log("Ejercicio 6: " + listaConcat.toString());
console.log("Ejercicio 6: " + listaConcat2.toString());
console.log("Ejercicio 6: " + listaConcat3.toString());
console.log("Ejercicio 6: " + listaConcat4.toString());
console.log("Ejercicio 6: " + listaConcat5.toString());

let listaDCBA = listaMasLarga.cons('a').cons('b').cons('c').cons('d');
let listaZX = nil.cons('x').cons('z');
let listaConcat6 = listaDCBA.concatenar(listaZX);
let listaConcat7 = listaZX.concatenar(listaDCBA);

console.log("Ejercicio 6 - " + listaConcat6.toString() + ": " + listaConcat6.length());
console.log("Ejercicio 6 - " + listaConcat6.toString() + ": " + listaConcat6.length());

console.log("Ejercicio 6 - " + listaConcat7.toString() + ": " + listaConcat7.length());
console.log("Ejercicio 6 - " + listaConcat7.toString() + ": " + listaConcat7.length());