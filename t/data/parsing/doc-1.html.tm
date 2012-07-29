<!DOCTYPE html>
<html>
  <link rel=stylesheet href=/foo/bar.css>
  <title t:parse><t:text value=" foo () "></title>
  <t:macro name=text>
    <t:text value=bar()>
  </t:macro>
  <p><m:text/>
</html>
