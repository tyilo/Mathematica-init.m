Mathematica-init.m
==================

Installing
-------------

Place in `init.m` in `ToFileName[{$UserBaseDirectory,"Kernel"},"init.m"]` and restart Mathematica.

Or you could just run this in Mathematica:

```
newest = URLFetch@"https://raw.github.com/Tyilo/Mathematica-init.m/master/init.m";
initPath = ToFileName[{$UserBaseDirectory, "Kernel"}, "init.m"];
WriteString[f = OpenWrite@initPath, newest];
Close@f;
```

Updating
-------------

Once you have the init file installed, you can call the function `updateInitFile[]` to update the `init` file to the latest version.

Changes to preferences
--------------------------

View this imgur album for seeing the changes that I've made to the default Preferences in Mathematica: <http://imgur.com/a/ko9A4>
