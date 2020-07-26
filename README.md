# TypeGettingLanguage

[*Cpp* Version](https://github.com/Talaodi/TypeGettingLanguage)

### An Example

**test.tgl:**

```cpp
{
	int x;
	char y;
	{
		y;
		bool y;
		x;
		y;
	}
	y;
}
```

```sh
Main test.tgl
```

**output:**

```cpp
{
        {
                y : char
                x : int
                y : bool
        }
        y : char
}
```





