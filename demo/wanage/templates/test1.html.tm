<!DOCTYPE html>
<html>
<title t:parse>
  My test document <t:text value="2 * 10 + 20 -39">
</title>

<p>hoge fuga</p>

<t:for as=i x=[1..1000]>
  <p><t:text value=$i> <t:text value="$i * 2">
</t:for>
