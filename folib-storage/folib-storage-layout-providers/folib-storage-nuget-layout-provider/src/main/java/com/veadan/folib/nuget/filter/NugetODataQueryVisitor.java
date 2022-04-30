package com.veadan.folib.nuget.filter;

import com.veadan.folib.artifact.criteria.ArtifactEntryCriteria;
import com.veadan.folib.data.criteria.Expression;
import com.veadan.folib.data.criteria.Predicate;
import com.veadan.folib.data.criteria.Selector;
import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.artifact.ArtifactTag;

/**
 * This class purpose is to construct {@link Predicate} instance which can be
 * used with {@link Selector} to perform Database queries.
 * 
 * Every method here should produce {@link Predicate} instance according to
 * filter expression it visit.
 * 
 * @author sbespalov
 *
 */
public class NugetODataQueryVisitor extends NugetODataFilterBaseVisitor<Predicate>
{

    /**
     * The Root predicate in the parse tree.
     */
    private Predicate root = Predicate.empty();

    public NugetODataQueryVisitor()
    {
        super();
    }

    @Override
    public Predicate visitFilter(NugetODataFilterParser.FilterContext ctx)
    {
        Predicate p = super.visitFilter(ctx);
        return root.and(p);
    }

    @Override
    public Predicate visitFilterExp(NugetODataFilterParser.FilterExpContext ctx)
    {
        if (ctx.tokenExp() != null)
        {
            return visitTokenExp(ctx.tokenExp());
        }
        else if (ctx.vNestedFilterExp != null)
        {
            return visitFilterExp(ctx.vNestedFilterExp);
        }

        Predicate.BooleanOperator booleanOperator = Predicate.BooleanOperator.valueOf(ctx.vLogicalOp.getText().toUpperCase());

        Predicate filterExpRoot = Predicate.empty();

        Predicate p1 = visitFilterExp(ctx.vFilterExpLeft);
        Predicate p2 = visitFilterExp(ctx.vFilterExpRight);

        if (Predicate.BooleanOperator.AND.equals(booleanOperator))
        {
            filterExpRoot.and(p1).and(p2);
        }
        else if (Predicate.BooleanOperator.OR.equals(booleanOperator))
        {
            filterExpRoot.or(p1).or(p2);
        }

        return filterExpRoot;
    }

    @Override
    public Predicate visitTokenExp(NugetODataFilterParser.TokenExpContext ctx)
    {
        if (ctx.TAG() != null)
        {
            ArtifactEntryCriteria c = new ArtifactEntryCriteria();
            c.getTagSet().add(ArtifactTag.LAST_VERSION);

            return Predicate.of(Expression.ExpOperator.CONTAINS.of("tagSet.name", ArtifactTag.LAST_VERSION));
        }

        Predicate p = visitTokenExpLeft(ctx.vTokenExpLeft);

        String attributeValue = ctx.vTokenExpRight.getText();
        attributeValue = StringUtils.unwrap(attributeValue, "'");
        p.getExpression().setValue(attributeValue);

        return p;
    }

    @Override
    public Predicate visitTokenExpLeft(NugetODataFilterParser.TokenExpLeftContext ctx)
    {
        if (ctx.ATTRIBUTE() != null)
        {
            String attribute = ctx.ATTRIBUTE().getText();
            return Predicate.of(Expression.ExpOperator.EQ.of(String.format("artifactCoordinates.coordinates.%s",
                                                                attribute.toLowerCase()),
                                                  null));
        }
        return visitTokenExpFunction(ctx.tokenExpFunction());
    }

    @Override
    public Predicate visitTokenExpFunction(NugetODataFilterParser.TokenExpFunctionContext ctx)
    {
        String attribute = ctx.ATTRIBUTE().getText().toLowerCase();

        if (ctx.fuctionExp().TO_LOWER() != null)
        {
            attribute = String.format("%s.toLowerCase()", attribute);
        }

        return Predicate.of(Expression.ExpOperator.EQ.of(String.format("artifactCoordinates.coordinates.%s",
                                                            attribute),
                                              null));
    }

}
