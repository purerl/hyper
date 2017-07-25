**********
Middleware
**********

A *middleware* is an *indexed monadic action* transforming one ``Conn``
to another ``Conn``. It operates in some base monad ``m``, and is
indexed by ``i`` and ``o``, the *input* and *output* Conn types of the
middleware action.

.. code-block:: haskell

    newtype Middleware m i o a = ...

The input and output type parameters are used to ensure that a Conn is
transformed, and that side-effects are performed, correctly, throughout
the middleware chain.

Being able to parameterize ``Middleware`` with some type ``m``, you can
customize the chain depending on the needs of your middleware and
handlers. Applications can use monad transformers to track state,
provide configuration, gather metrics, and much more, in the chain of
middleware.

Middleware are composed using ``ibind``, the indexed monadic version of
``bind``. The simplest way of composing middleware is by chaining them
with ``:*>``, from ``Control.IxMonad``. See
`haskell-indexed-monad <https://pursuit.haskell.org/packages/haskell-indexed-monad/0.1.1>`__
for more information.

.. code-block:: haskell

    writeStatus statusOK
    :*> closeHeaders
    :*> respond "We're composing middleware!"

If you want to feed the return value of one middleware into another, use
``:>>=``, the infix operator alias for ``mbind``.

.. code-block:: haskell

    getUser :>>= renderUser

You can also rebind the *do block* syntax to use ``ibind`` instead of
regular ``bind``.

.. code-block:: haskell

    do
      user <- getUser
      writeStatus statusOK
      closeHeaders
      respond ("User: " <> user.name)
      where bind = ibind

