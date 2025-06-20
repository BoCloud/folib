package com.veadan.folib.aql.grammar;

import com.veadan.folib.data.criteria.Expression;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.data.criteria.Predicate;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.aql.grammar.AQLParser.QueryContext;
import com.veadan.folib.aql.grammar.AQLParser.QueryExpContext;
import com.veadan.folib.domain.ArtifactEntity;

/**
 * @author xuxinping
 *
 */
public class AqlStatementVisitor extends AQLBaseVisitor<Selector<ArtifactEntity>>
{

    private Selector<ArtifactEntity> selector = new Selector<>(ArtifactEntity.class);

    public AqlStatementVisitor()
    {
    }

    @Override
    public Selector<ArtifactEntity> visitQuery(QueryContext ctx)
    {
        Predicate artifactPredicate = Predicate.of(Expression.ExpOperator.IS_NOT_NULL.of("artifactCoordinates"));
        AqlQueryVisitor queryVisitor = new AqlQueryVisitor(artifactPredicate);

        for (QueryExpContext queryExpContext : ctx.queryExp())
        {
            artifactPredicate.and(queryVisitor.visitQueryExp(queryExpContext).nested());
        }
        selector.where(queryVisitor.getRoot());

        AqlPaginatorVisitor aqlPaginatorVisitor = new AqlPaginatorVisitor();
        Paginator paginator = aqlPaginatorVisitor.visitOrderExp(ctx.orderExp());
        paginator = aqlPaginatorVisitor.visitPageExp(ctx.pageExp());

        selector.with(paginator);

        return selector;
    }

}
