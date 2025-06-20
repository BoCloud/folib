package com.veadan.folib.aql.grammar;

import java.util.Optional;

import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.aql.grammar.AQLParser.OrderExpContext;
import com.veadan.folib.aql.grammar.AQLParser.PageExpContext;

/**
 * @author xuxinping
 *
 */
public class AqlPaginatorVisitor extends AQLBaseVisitor<Paginator>
{

    private Paginator paginator = new Paginator();

    public AqlPaginatorVisitor()
    {
        super();
        paginator.setLimit(25);
    }

    @Override
    public Paginator visitPageExp(PageExpContext ctx)
    {
        if (ctx == null)
        {
            return paginator;
        }
        paginator.setSkip(Long.valueOf(ctx.NUMBER().getText()));

        return paginator;
    }

    @Override
    public Paginator visitOrderExp(OrderExpContext ctx)
    {
        if (ctx == null)
        {
            return paginator;
        }

        if (Paginator.Order.DESC.toString().equalsIgnoreCase(ctx.orderDirection().getText()))
        {
            paginator.setOrder(Paginator.Order.DESC);
        }

        String aqlOrderProperty = ctx.orderValue().getText();

        for (AqlMapping aqlPropertyKeyword : AqlMapping.values())
        {
            if (!aqlPropertyKeyword.toString().equalsIgnoreCase(aqlOrderProperty))
            {
                continue;
            }

            paginator.setProperty(aqlPropertyKeyword.property());

            break;
        }

        paginator.setProperty(Optional.ofNullable(paginator.getProperty())
                                      .orElse(String.format("artifactCoordinates.coordinates.%s",
                                                            aqlOrderProperty)));

        return paginator;
    }

}
