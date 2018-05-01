Workshop: Domain Modelling with Haskell
=======================================

Welcome to the “Domain Modelling with Haskell” workshop! This material
is based on the screencast series from `Haskell at Work
<https://haskell-at-work.com>`__. It contains the instructions,
exercises, and references for you to get started with Haskell and
domain modelling, in the example domain of *project management*.

Introduction
------------

We will model a simple project management system, and implement
*reporting* functionality based on our core data structures. The
sections in this workshop explore increasingly abstract techniques for
working with Haskell data structures. We begin with regular Haskell data
structures, monoids, and recursion.

Attendees should be comfortable with Haskell basics, preferably at the
level of Haskell programming from first principles or similar. The
workshop goal of this workshop is to provide a step from theoretical
exercises into what tasks that are closer to "bread and butter"
programming in industry.

Setup
-----

Installing the GHC compiler
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you don’t have any strong preferences on your Haskell installation,
and just want to get going, you can use `Stack
<https://haskellstack.org>`__ to install the Haskell compiler, the
required libraries, and compiling the code. The website has the
`installation instructions
<https://docs.haskellstack.org/en/stable/README/#how-to-install>`_ on
the home page.

You can also use GHC and Cabal provided by `Haskell Platform
<https://www.haskell.org/platform/>`_, which has packages for many
operating systems and distributions.

Cloning and Installing Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To get you up and running, begin by cloning this repository to your
local machine using `Git <https://git-scm.com/>`_:

.. code:: sh

    $ git clone https://github.com/owickstrom/domain-modelling-with-haskell-workshop.git
    $ cd domain-modelling-with-haskell-workshop

Then, install all dependencies and build the project by running the
`stack build` command. It first installs the dependencies if missing,
then builds the project.

.. code:: sh

    $ stack build

If you are not using Stack, e.g. the Haskell Platform, run:

.. code:: sh

   $ cabal update
   $ cabal install --only-dependencies
   $ cabal configure
   $ cabal build

The Cabal file for this project, located at ``domain-modelling.cabal``
in the root directory, has a number of library dependencies
already. That way, you will have them installed when you start, and
complete the workshop without an internet connection.

All right! If all went well, you are ready to get started. You may edit
Haskell files in any text editor you like. If you don’t have any strong
preference, both `VS Code <https://code.visualstudio.com/>`__ and
`Atom <https://atom.io/>`__ are easy to get started with.

Setting up Haskell tooling is way out of the scope of this workshop,
and you will not need anything fancy. We will load and test the code
in GHCi (the interactive GHC environment) anyway. If you are used to
other functional programming languages, GHCi is equivalent of a `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`_.

Editing Workflow
----------------

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

When you want to check some code you have written, type ``:reload`` in
GHCi and hit Enter. If there are any type errors, GHCi will print
them.

.. code:: ghci

   > :reload
   Ok, one module loaded.
   Collecting type info for 1 module(s) ...


Part 1: Data Structures
-----------------------

You will begin by laying the groundwork for the project management
system. The system will work with hierarchies of *projects*,
*budgets*, *transactions*, and provide calculatation of *reports*,
showing a project's budgeted and actual profit. To keep the scope of
the workshop down, the system will be implemented with fake database
queries and will not have any graphical user interface.

Details and instructions about what you will build are listed
below. Go through this list in order and implement each part. The
nouns (or "things") should be modelled as data types, and operations and
queries as functions (using ``IO`` where needed.) Click the **TIP**
boxes to get some assistance if you need.

Okay, let's begin!

Implementation
~~~~~~~~~~~~~~

:Project:

   The core concept in the system is a *project*. A project can be
   either a single project or a project group. Both single projects
   and project groups have *names*, and single projects also have
   *project IDs* (described below). A project group has a list of
   child projects.

   .. tip::

      A data structure with multiple variants can be expressed using a
      data type with multiple *constructors:*

      .. code:: haskell

         data MyThing
           = RegularThing Int
           | OtherThing String

:Project ID:

   A project ID uniqely identifies a *single* project (non-group
   project) in the system. It is a `natural
   number <https://en.wikipedia.org/wiki/Natural_number>`_.

   .. note::

      Create a type for project IDs to make it more explicit, either
      using a `type` alias, or using a `newtype`.

   .. tip::

      A natural number can be represented using the `Word
      <http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Word.html>`_
      type, which is the equivalent of `unsigned int
      <https://en.wikipedia.org/wiki/C_data_types>`_ from C.

      .. code:: haskell

         type ProjectId = Word

         myProjectId :: ProjectId
         myProjectId = 1

   .. tip::

      By wrapping in a `newtype`, instead of using a "raw" numeric type
      or a type alias, you make it safer to pass around in the code, as
      it cannot be mistakenly swapped with other integers.

      .. code:: haskell

         newtype ProjectId = ProjectId Int
           deriving (Eq, Show)

      Note that we use `deriving`, a way of having the compiler
      automatically create instances for the listed type classes. In
      this case we get

      * an ``Eq`` instance, which lets us check if ``ProjectId``
        values are equal, and
      * a ``Show`` instance, which gives us a ``String``
        representation of a ``ProjectId`` value.

      When we use a newtype, the following code would cause a type
      error.

      .. code:: haskell

         pId :: ProjectId
         pId = ProjectId 123

         ohNo = pId * 4

:Budget:

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

:Transaction:

   A transaction is a very simplified concept describing a *sale* or a
   *purchase*. Each type of transaction has an amount (a monetary value.)

:Money:

   A representation of monetary values.

   .. note:: Represent ``Money`` using the ``Decimal`` type from the
      `Decimal
      <https://hackage.haskell.org/package/Decimal-0.5.1/docs/Data-Decimal.html>`_
      package. You do not need to care about currency, only the
      amount.

   .. tip::

      Create a ``newtype`` for monetary values by wrapping the
      ``Decimal`` type:

      .. code:: haskell

         module Project where

         import Data.Decimal

         newtype Money = Money Decimal
           deriving (Show, Eq)


:Get Budget by Project ID:

   Given a project ID, we need to be able to retrieve a budget for the
   corresponding project.

   .. note::

      To keep the scope of this workshop limited we will not use a
      real database. Instead, hard-code a result, making it a "fake"
      query. The function should still return ``IO``, e.g:

      .. code:: haskell

                getBudget :: ProjectId -> IO Budget

      If you want to use a real database, consider that a follow-up
      exercise.

:Get Transactions by Project ID:

   Given a project ID, we need to be able to retrieve a list of
   transactions for the corresponding project.

   .. note::

      Again, to keep the scope of this workshop limited we will not
      use a real database. Instead, hard-code a result, making it a
      "fake" query. The function should still return ``IO``, e.g:

       .. code:: haskell

                 getTransactions :: ProjectId -> IO [Transaction]

:Report:

   A report represents the result of the *report calculation*. It has a
   *budget profit*, a *net profit*, and a *difference*, all being
   monetary values.

:Calculate Report:

   Create a report from a budget and a list of transactions. It
   calculates a report, where:

   .. math::

      \text{budget profit} = \text{income} - \text{expenditure}

      \text{net profit} = \text{sales} - \text{purchases}

      \text{difference} = \text{net profit} - \text{budget profit}

   .. note::

      The report calculation function should be a pure function,
      i.e. not using ``IO``. Give it a type signature like:

      .. code:: haskell

         calculateReport :: Budget -> [Transaction] -> Report

   .. tip::

      Derive an instance of the ``Num`` class for ``Money`` so that
      you can use regular arithmetic operators in your calculation.
      To do this you also need enable the
      ``GeneralizedNewtypeDeriving`` extension.

      .. code:: haskell

         {-# LANGUAGE GeneralizedNewtypeDeriving #-}
         module Project where

         -- other code ...

         newtype Money = Money Decimal
           deriving (Show, Eq, Num)

      Now you can add money values together:

      .. code:: haskell

         pocketMoney = Money 123
         savings = Money 4567

         allMyCash = pocketMoney + savings

   .. tip::

      Consider folding over the list of transactions using a function
      from `Data.Foldable
      <http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Foldable.html>`_.
      You can use `foldl'`, or ``foldMap`` if you can find a ``Monoid``
      that sums money values.

   .. tip::

      There is a ``Monoid`` instance for ``Sum``, that sums the
      wrapped values using the ``+`` operator. You can use ``getSum``
      and ``foldMap`` to map the list of transactions into a list of
      positive or negative ``Money`` values, and sum them using
      ``Sum``.

      .. code:: haskell

        netProfit' = getSum (foldMap asProfit transactions)
        asProfit (Sale m)     = pure m
        asProfit (Purchase m) = pure (negate m)


:Calculate Project Report:

   Given a project, this function calculates a single aggregated report
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
         calculateProjectReport (SingleProject projectId _) = _
         calculateProjectReport (ProjectGroup _ projects) = _

      and by folding the result of recursively applying
      ``calculateProjectReport`` on project group children:

      .. code:: haskell

         foldMap calculateProjectReport _childProjects

Testing Your Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whew! Those are all the things needed in the project management
system. To try report calculation, define ``someProject :: Project``
at the top-level in ``Project.hs``. You may construct this value
however you like, but make sure to have at least two levels of project
groups.

.. tip::

   As an example of a project value, the following represents a
   Swedish project hierarchy of some sort.

   .. code:: haskell

      someProject :: Project
      someProject = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
        where
          stockholm = SingleProject (ProjectId 1) "Stockholm"
          gothenburg = SingleProject (ProjectId 2) "Gothenburg"
          malmo = ProjectGroup "Malmö" [city, limhamn]
          city = SingleProject (ProjectId 3) "Malmö City"
          limhamn = SingleProject (ProjectId 4) "Limhamn"

To construct ``Text`` values with regular string literals, enable the
``OverloadedStrings`` extension:

.. code:: haskell

   {-# LANGUAGE OverloadedStrings          #-}
   module Project where

   -- your previous code...

   someProject = ProjectGroup "My Group" []

Now, go back to GHCi, reload, and apply the report calculation
function to the ``someProject`` value. Do you get a single report
back?

.. tip::

   To calculate a report in GHCi, run something like the following,
   and you should see the report data structure printed.

   .. code:: ghci

      > :reload
      λ> calculateProjectReport someProject
      Report {budgetProfit = Money 800, netProfit = Money -568, difference = Money -1368}

Congratulations! You have completed the first part of "Domain
Modelling with Haskell."

Bonus Exerices
~~~~~~~~~~~~~~

* Render the report by creating a `Tree
  <https://hackage.haskell.org/package/containers-0.5.11.0/docs/Data-Tree.html>`_
  from a report, and rendering that using ``drawTree``.
* Convert the hard-coded queries to generate random values using
  `System.Random
  <http://hackage.haskell.org/package/random-1.1/docs/System-Random.html>`_.
* Convert the hard-coded or random-generated query results to use a
  real database, e.g. PostgreSQL or MySQL. Haskell has many options
  for working with relational databases.

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

Credits
-------

A huge thanks to
`@vbhvsgr <https://twitter.com/vbhvsgr>`_,
`@evanborden <https://twitter.com/evanborden>`_,
`@trupill <https://twitter.com/trupill>`_,
`@paulcadman <https://twitter.com/paulcadman>`_,
`@themattchan <https://twitter.com/themattchan>`_,
and `@mathapptician <https://twitter.com/mathapptician>`_
for proof-reading and giving feedback on this material!
