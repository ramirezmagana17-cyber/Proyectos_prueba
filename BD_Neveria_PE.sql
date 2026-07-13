
CREATE DATABASE IF NOT EXISTS neveria_sistema;
USE neveria_sistema;

CREATE TABLE categorias (
    id_categoria INT AUTO_INCREMENT PRIMARY KEY,
    nombre_categoria VARCHAR(50) NOT NULL,
    descripcion TEXT
);

CREATE TABLE productos (
    id_producto INT AUTO_INCREMENT PRIMARY KEY,
    nombre_producto VARCHAR(100) NOT NULL,
    precio_unitario DECIMAL(8, 2) NOT NULL,
    id_categoria INT,
    CONSTRAINT fk_categoria_producto FOREIGN KEY (id_categoria) 
        REFERENCES categorias(id_categoria)
        ON DELETE SET NULL
);

CREATE TABLE empleados (
    id_empleado INT AUTO_INCREMENT PRIMARY KEY,
    nombre VARCHAR(100) NOT NULL,
    puesto VARCHAR(50),
    fecha_ingreso DATE DEFAULT (CURRENT_DATE)
);

CREATE TABLE ventas (
    id_venta INT AUTO_INCREMENT PRIMARY KEY,
    fecha_venta DATETIME DEFAULT CURRENT_TIMESTAMP,
    id_empleado INT,
    total_venta DECIMAL(10, 2) DEFAULT 0.00
);

CREATE TABLE detalle_ventas (
    id_detalle INT AUTO_INCREMENT PRIMARY KEY,
    id_venta INT NOT NULL,
    id_producto INT NOT NULL,
    cantidad INT NOT NULL,
    subtotal DECIMAL(10, 2) NOT NULL
);

ALTER TABLE ventas
    ADD CONSTRAINT fk_venta_empleado
    FOREIGN KEY (id_empleado) REFERENCES empleados(id_empleado);
    
    ALTER TABLE detalle_ventas
    ADD CONSTRAINT fk_detalle_venta
    FOREIGN KEY (id_venta) REFERENCES ventas(id_venta)
    ON DELETE CASCADE;
    
    ALTER TABLE detalle_ventas
    ADD CONSTRAINT fk_detalle_producto
    FOREIGN KEY (id_producto) REFERENCES productos(id_producto);
    