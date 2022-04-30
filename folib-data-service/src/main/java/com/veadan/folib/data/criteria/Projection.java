package com.veadan.folib.data.criteria;

/**
 * @author sbespalov
 *
 */
public enum Projection
{
    ROWS("%s"),
    COUNT("count(%s)");

    private Projection(String token)
    {
        this.token = token;
    }

    private String token;

    public String token() {
        return token("*");
    }
    
    public String token(String param)
    {
        return String.format(token, param);
    }
}
