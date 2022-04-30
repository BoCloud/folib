package com.veadan.folib.dependency.snippet;

import javax.annotation.Nonnull;
import java.util.Objects;

public class CodeSnippet
    implements Comparable<CodeSnippet>
{
    
    protected String name;
    
    protected String code;
    
    
    public CodeSnippet(String name, String code)
    {
        this.name = name;
        this.code = code;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getCode()
    {
        return code;
    }

    public void setCode(String code)
    {
        this.code = code;
    }

    @Override
    public int compareTo(@Nonnull CodeSnippet codeSnippet)
    {
        return name.compareTo(codeSnippet.getName());
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        CodeSnippet snippet = (CodeSnippet) o;

        return Objects.equals(name, snippet.name) && Objects.equals(code, snippet.code);
    }

    @Override
    public int hashCode()
    {

        return Objects.hash(name, code);
    }

}
