package com.veadan.folib.data.criteria;

/**
 * @author sbespalov
 *
 */
public class Expression
{

    private String property;
    private ExpOperator operator = ExpOperator.EQ;
    private Object value;

    Expression()
    {

    }

    public Expression(String property,
                      Object value)
    {
        this(property, ExpOperator.EQ, value);
    }

    public Expression(String property,
                      ExpOperator operator,
                      Object value)
    {
        super();
        this.property = property;
        this.operator = operator;
        this.value = value;
    }

    public String getProperty()
    {
        return property;
    }

    public void setProperty(String property)
    {
        this.property = property;
    }

    public ExpOperator getOperator()
    {
        return operator;
    }

    public void setOperator(ExpOperator operator)
    {
        this.operator = operator;
    }

    public Object getValue()
    {
        return value;
    }

    public void setValue(Object value)
    {
        this.value = value;
    }

    public enum ExpOperator
    {
        EQ, GE, LE, CONTAINS, LIKE, IS_NULL, IS_NOT_NULL;

        public Expression of(String property,
                             Object value)
        {
            return new Expression(property, this, value);
        }

        public Expression of(String property)
        {
            return new Expression(property, this, null);
        }
        
    }
    
}
