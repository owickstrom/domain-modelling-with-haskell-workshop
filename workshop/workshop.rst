Workshop: Domain Modelling with Haskell
=======================================

Welcome to the “Domain Modelling with Haskell” workshop, based off the
screencast series from `Haskell at
Work <https://haskell-at-work.com>`__. This document contains the
instructions, exercises, and references for you to get started with
Haskell and domain modelling, in the example domain of *project
management*.

Introduction
------------

We will model a simple project management system, and implement
*reporting* functionality based on our core data structures. The
sections in this workshop explore increasingly abstract techniques for
working with Haskell data structures. We begin with regular Haskell data
structures, monoids, and explicit recursion.

Setup
-----

Installing Stack
~~~~~~~~~~~~~~~~

If you don’t have any strong preferences on your Haskell installation,
and just want to get going, it is recommended to use
`Stack <https://haskellstack.org>`__ to install the Haskell compiler,
the required libraries, and compiling the code. The website has the
installation instructions on the home page.

If you do install GHC and Cabal in another way, do consider using Cabal
``new-build``, a sandbox, or some other means of isolating the build.

Cloning and Installing Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To get you up and running, begin by cloning this repository to your
local machine:

.. code:: sh

    $ git clone https://github.com/owickstrom/domain-modelling-with-haskell-workshop.git
    ...
    $ cd domain-modelling-with-haskell-workshop

Then, install all dependencies required, and build the project:

.. code:: sh

    $ stack build

The Cabal file for this project has a number of library dependencies
already, so that you can have them installed beforehand, and complete
the workshop without an (or with a flaky) internet connection.

All right! If all went well, you are ready to get started. You may edit
Haskell files in any text editor you like. If you don’t have any strong
preference, both `VS Code <https://code.visualstudio.com/>`__ and
`Atom <https://atom.io/>`__ are easy to get started with.

Setting up Haskell tooling is way out of the scope of this workshop, and
you will not need anything fancy. We will load and test the code in GHCi
(a REPL) anyway.

How to follow this tutorial
---------------------------

When you are done with the setup, begin by running the following
command in your terminal. It will start GHCi and load the `Project`
module.

.. code:: sh

   $ stack repl

Open your text editor and navigate to the workshop Git
repository. Write all your code in ``src/Project.hs`` under the module
header:

.. code:: haskell

   module Project where

   -- Your stuff here!

When you want to check some code you have written, write ``:reload``
and hit Enter. If there are any type errors, GHCi will print them.

.. code:: ghci

   > :reload
   Ok, one module loaded.
   Collecting type info for 1 module(s) ...


Part 1: Data Structures
-----------------------

You will begin by laying the groundwork for the project management
system. The concepts and features that we need in the are listed and
described informally below.

Go through this list in order and implement each part. The "things"
should be modelled as data types, and operations and queries as
functions (using ``IO`` where needed.) Click the **TIP** boxes to get
some assistance if you need.

Project
~~~~~~~

The core concept in the system is a *project*. A project can be
either a single project or a project group. Both single projects and
project groups have *names*, and single projects also have *project
IDs*, which are natural numbers. A project group has a list of child
projects.

.. tip::

   A data structure with multiple variants can be expressed using a
   data type with multiple *constructors:*

   .. code:: haskell

      data MyThing
        = RegularThing Int
        | OtherThing String

Project ID
~~~~~~~~~~

A project ID uniqely identifies a *single* project (non-group
project) in the system.

.. note::

   Create a type for project IDs to make it more explicit, either
   using a `type` alias, or using a `newtype`.

.. tip::

   By wrapping in a `newtype`, instead of using a "raw" numeric type
   or a type alias, you make it safer to pass around in the code, as
   it cannot be mistakenly interchanged with other integers.

   .. code:: haskell

      newtype ProjectId = ProjectId { unProjectId :: Int }
        deriving (Eq, Show)

   The following code would then cause a type error.

   .. code:: haskell

      pId :: ProjectId
      pId = ProjectId 123

      ohNo = pId * 4

Budget
~~~~~~

A budget describes the expected *income* and *expenditure* for a
project, both being monetary values.

.. tip::

   A Haskell data type with multiple fields can be expressed using
   record syntax:

   .. code:: haskell

      data Budget = Budget
        { budgetIncome :: Money
        , budgetExpenditure :: Money
        } deriving (Show, Eq)

Transaction
~~~~~~~~~~~

A transaction is a very simplified concept describing a *sale* or a
*purchase*. Each type of transaction has an amount (a monetary value.)

Money
~~~~~

A representation of monetary values.

.. note:: Represent ``Money`` using the ``Decimal`` type from the
   `Decimal`_ package.

.. _Decimal: https://hackage.haskell.org/package/Decimal-0.5.1/docs/Data-Decimal.html

.. tip::

   Create a ``newtype`` for monetary values by wrapping the
   ``Decimal`` type:

   .. code:: haskell

      module Project where

      import Data.Decimal

      newtype Money = Money
        { unMoney :: Decimal
        } deriving (Show, Eq, Num)


Get Budget by Project ID
~~~~~~~~~~~~~~~~~~~~~~~~

Given a project ID, we need to be able to retrieve a budget for the
corresponding project.

.. note::

   To save time, hard-code or generate a random result,
   instead of using a real persistent database. The function should
   still return ``IO``, e.g:

   .. code:: haskell

             getBudget :: ProjectId -> IO Budget

Get Transactions by Project ID
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a project ID, we need to be able to retrieve a list of
transactions for the corresponding project.

.. note::

    To save time, hard-code or generate a random result, instead of
    using a real persistent database or querying an external
    system. The function should still return ``IO``, e.g:

    .. code:: haskell

              getTransactions :: ProjectId -> IO [Transaction]

Report
~~~~~~

A report represents the result of the *report calculation*. It has a
*budget profit*, a *net profit*, and a *difference*, all being
monetary values.

Calculate Report
~~~~~~~~~~~~~~~~

The reporting calculation, depending on a project budget and a list
of project transactions. It calculates a `Report`_, where:

.. math::

   \text{budget profit} = \text{income} - \text{expenditure}

   \text{net profit} = \text{sales} - \text{purchases}

   \text{difference} = \text{net profit} - \text{budget profit}

.. note::

   The report calculation function should be a pure function, with a
   Haskell type signature like:

   .. code:: haskell

      calculateReport :: Budget -> [Transaction] -> Report

Calculate Project Report
~~~~~~~~~~~~~~~~~~~~~~~~

Given a project, this function calculates a single aggregated `Report`_
for the entire project hierarchy. It needs to recursively walk the
projects, query their budgets and transactions, calculate reports, and
combine those reports into one.

.. note::

   The project report calculation function returns ``IO Report``, e.g.:

   .. code:: haskell

      calculateProjectReport :: Project -> IO Report

   Use the (fake) queries you wrote earlier to obtain a budget and a
   list of transactions for each project.

.. tip::

   Create an instance of ``Monoid`` for ``Report`` and use it to
   combine reports:

   .. code:: haskell

      instance Monoid Report where
        mempty = Report 0 0 0
        mappend (Report b1 n1 d1) (Report b2 n2 d2) =
          Report (b1 + b2) (n1 + n2) (d1 + d2)

   Now you can combine a list of reports using ``fold``:

   .. code:: haskell

      megaReport :: Report
      megaReport = fold [report1, report2, report3]

.. tip::

   Recurse through the project hierarchy by pattern matching on the
   constructors,

   .. code:: haskell

      calculateProjectReport :: Project -> IO Report
      calculateProjectReport (SingleProject projectId _) = ...
      calculateProjectReport (ProjectGroup _ projects) = ...

   and by folding the result of recursively applying
   ``calculateProjectReport`` on project group children:

   .. code:: haskell

      foldMap calculateProjectReport _childProjects

Testing it all out
~~~~~~~~~~~~~~~~~~

Whew! Those are all the things needed in the project management
system. To have some data to try report calculation on, create
a ``someProject`` definition of type ``Project``. You may construct
this value however you like, but make sure to have at least three
levels of project groups.

.. tip::

   As an example of a project value, the following represents a
   Swedish project hierarchy of some sort.

   .. code:: haskell

             someProject :: Project
             someProject = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
               where
                 stockholm = Project (ProjectId 1) "Stockholm"
                 gothenburg = Project (ProjectId 2) "Gothenburg"
                 malmo = ProjectGroup "malmo" [city, limhamn]
                 city = Project (ProjectId 3) "Malmo City"
                 limhamn = Project (ProjectId 4) "Limhamn"

Now, apply the report calculation function to the demo project. Do
you get a single report back?

.. tip::

   To calculate a report in the GHCi REPL, run something like the
   following, and you should see the report data structure printed.

   .. code:: ghci

             > :reload
             > calculateProjectReport someProject
             Report {budgetProfit = Money {unMoney = -5392.74046336179},
             netProfit = Money {unMoney = 2191.2802854168813}, difference =
             Money {unMoney = 7584.020748778671}}

Congratulations! You have completed the first part of "Domain
Modelling with Haskell."

Digging Deeper
--------------

This workshop is based on the video series from Haskell at Work:

1. `Data Structures <https://haskell-at-work.com/episodes/2018-01-19-domain-modelling-with-haskell-data-structures.html>`_
1. `Generalizing with Foldable and Traversable <https://haskell-at-work.com/episodes/2018-01-22-domain-modelling-with-haskell-generalizing-with-foldable-and-traversable.html>`_
1. `Accumulating with WriterT <https://haskell-at-work.com/episodes/2018-02-02-domain-modelling-with-haskell-accumulating-with-writert.html>`_
1. `Factoring Out Recursion <https://haskell-at-work.com/episodes/2018-02-11-domain-modelling-with-haskell-factoring-out-recursion.html>`_

**Parts 2-4 have not been added to this workshop yet.** If you want to
explore further, I can recommend checking out those videos and the
show notes. Also, the full source code for the videos is available
at
https://github.com/haskell-at-work/domain-modelling-with-haskell.
