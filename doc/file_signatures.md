

# Module file_signatures #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_type-2">is_type/2</a></td><td>
Return <tt>ok</tt> if <tt>Filename</tt> has a <tt>Type</tt> signature.</td></tr><tr><td valign="top"><a href="#is_valid-1">is_valid/1</a></td><td>
Return <tt>ok</tt> if <tt>Filename</tt> is a valid signature according to is extension.</td></tr><tr><td valign="top"><a href="#signature-1">signature/1</a></td><td>
Return the first matching signature for <tt>Filename</tt>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_type-2"></a>

### is_type/2 ###

<pre><code>
is_type(Filename::<a href="file.md#type-name_all">file:name_all()</a>, Type::atom() | [atom()]) -&gt; ok | {error, term()}
</code></pre>
<br />

Return `ok` if `Filename` has a `Type` signature.

Example:

```

 file_signatures:is_type("sample.png", png).
 file_signatures:is_type("sample.png", [gif, jpeg, bmp, png])
```

<a name="is_valid-1"></a>

### is_valid/1 ###

<pre><code>
is_valid(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Return `ok` if `Filename` is a valid signature according to is extension.

Example:

```

 file_signatures:is_valid("sample.png").
```

<a name="signature-1"></a>

### signature/1 ###

<pre><code>
signature(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; atom() | undefined
</code></pre>
<br />

Return the first matching signature for `Filename`.

Example:

```

 file_signatures:signature("sample.png").
```

