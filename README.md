# Warehouse Receiving & Dispatch System

A COBOL-based warehouse management system developed using **GnuCOBOL**. The system manages stock items, records incoming and outgoing stock movements, monitors reorder levels, generates movement reports, and handles SKU discontinuation.

## Project Overview

The Warehouse Receiving & Dispatch System provides basic warehouse inventory management functionality through a menu-driven COBOL application.

The system maintains two indexed files:

* `STOCKS.DAT` — stores current warehouse stock information.
* `MOVEMENTS.DAT` — stores all stock receipt and dispatch movements.

## Features

### 1. Add New SKU

Allows the user to create a new stock item with:

* SKU
* Description
* Unit of measure
* Quantity on hand
* Reorder point
* Warehouse location
* Status

New SKUs are automatically assigned the status `ACTIVE`.

### 2. Record Goods Receipt

Records incoming stock and:

* Validates that the SKU exists.
* Prevents receipts for discontinued SKUs.
* Increases quantity on hand.
* Creates a movement record.
* Generates a unique movement ID.
* Records the GRN/reference number.
* Records the quantity after the movement.

### 3. Record Dispatch

Records stock dispatched to customers and:

* Validates that the SKU exists.
* Prevents dispatches for discontinued SKUs.
* Validates that sufficient stock is available.
* Prevents dispatch quantities greater than available stock.
* Decreases quantity on hand.
* Creates a movement record.
* Records the delivery note/reference number.
* Records the quantity after the movement.

### 4. Look Up Current Stock

Allows the user to enter an SKU and view:

* SKU
* Description
* Unit of measure
* Quantity on hand
* Reorder point
* Warehouse location
* Status

### 5. List SKUs Below Reorder Point

Lists all active stock items whose quantity on hand is below their reorder point.

The results are sorted using the COBOL `SORT` verb by:

1. Warehouse location
2. SKU

### 6. Stock Movement Report

Generates a stock movement report for a specified date range.

The report provides, per SKU:

* Total receipts
* Total dispatches
* Net movement

Net movement is calculated as:

```text
Receipts - Dispatches
```

The report includes pagination for larger result sets.

### 7. Discontinue SKU

Allows an SKU to be discontinued when:

* The SKU exists.
* The quantity on hand is exactly zero.
* The user confirms the discontinuation.

Once discontinued, the SKU cannot receive or dispatch additional stock.

## Program Structure

```text
CAPSTONE/
│
├── WAREHOUSE-MAIN.CBL
├── STOCK-RECORD.CPY
├── VALIDATE-STOCK.CBL
├── CALC-NET-MOVEMENT.CBL
├── INIT-WAREHOUSE.CBL
│
├── STOCKS.DAT
├── MOVEMENTS.DAT
│
└── README.md
```

## Required Files

### STOCKS.DAT

Indexed stock file using:

```text
Key: SKU
PIC X(10)
```

The stock record contains:

```text
SKU
Description
Unit of Measure
Quantity on Hand
Reorder Point
Warehouse Location
Status
```

### MOVEMENTS.DAT

Indexed movement file using:

```text
Key: Movement ID
PIC X(12)
```

The movement record contains:

```text
Movement ID
SKU
Movement Type
Date
Quantity
Reference Number
Quantity After Movement
```

## COPY Member

### STOCK-RECORD.CPY

Contains the shared stock record definition:

```text
STOCK-SKU
STOCK-DESCRIPTION
STOCK-UOM
STOCK-QTY-ON-HAND
STOCK-REORDER-POINT
STOCK-LOCATION
STOCK-STATUS
```

## Subprograms

### VALIDATE-STOCK.CBL

Validates whether sufficient stock exists for a dispatch.

The validation checks:

```text
Quantity on Hand >= Dispatch Quantity
```

It returns whether the dispatch is valid.

### CALC-NET-MOVEMENT.CBL

Calculates net stock movement using:

```text
Net Movement = Total Receipts - Total Dispatches
```

## Technologies

* COBOL
* GnuCOBOL
* Indexed Sequential Files
* COBOL `SORT`
* COBOL subprograms
* COBOL `COPY` members
* Linux/Ubuntu
* Git and GitHub

## Compilation

Compile the main program together with the two subprograms:

```bash
cobc -free -x WAREHOUSE-MAIN.CBL VALIDATE-STOCK.CBL CALC-NET-MOVEMENT.CBL -o WAREHOUSE-MAIN
```

## Running the System

Start the application with:

```bash
./WAREHOUSE-MAIN
```

The main menu is:

```text
==========================================
     WAREHOUSE RECEIVING & DISPATCH
==========================================
1. Add New SKU
2. Record Goods Receipt
3. Record Dispatch
4. Look Up Current Stock
5. List SKUs Below Reorder Point
6. Stock Movement Report
7. Discontinue SKU
8. Exit
==========================================
```

## Initialising the Data Files

If the warehouse data files need to be recreated, compile and run the initialization program:

```bash
cobc -free -x INIT-WAREHOUSE.CBL -o INIT-WAREHOUSE
```

Then:

```bash
./INIT-WAREHOUSE
```

**Note:** Running the initialization program may recreate or reset the test data files. Use it only when a fresh data set is required.

## Testing Performed

The following functionality was tested successfully:

### Add SKU

A new SKU was created successfully with its initial quantity, reorder point, location, and `ACTIVE` status.

### Stock Lookup

The system successfully displayed the current stock details for an SKU.

### Goods Receipt

A receipt was tested and the system correctly increased the quantity on hand and created a movement record.

### Dispatch

A valid dispatch successfully reduced the quantity on hand.

An attempted dispatch exceeding available stock was rejected with an `INSUFFICIENT STOCK` message.

### Reorder Report

SKUs below their reorder point were successfully displayed and sorted by warehouse location and SKU.

### Movement Report

The movement report successfully accepted a date range and displayed:

```text
SKU
Receipts
Dispatches
Net Movement
```

The net movement calculation was verified using:

```text
Receipts - Dispatches
```

### SKU Discontinuation

The system correctly rejected discontinuation when stock was greater than zero.

A zero-stock SKU was successfully discontinued after confirmation.

The system subsequently prevented both receipts and dispatches for the discontinued SKU.

## Example

For a SKU with:

```text
Receipts:    100
Dispatches:   30
```

the system calculates:

```text
Net Movement = 100 - 30
             = +70
```

## GitHub Repository

The project source code is maintained in the GitHub repository:

**Warehouse Receiving & Dispatch System**

Repository owner: `Mungad`

## Author

**Daisy Munga**

Developed as a COBOL warehouse management system project.
