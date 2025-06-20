package com.veadan.folib.nuget.filter;

import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.domain.ArtifactEntity;

/**
 * @author xuxinping
 *
 */
public class NugetODataFilterVisitorImpl extends NugetODataFilterBaseVisitor<Selector<ArtifactEntity>>
{

    private Selector<ArtifactEntity> selector = new Selector<>(ArtifactEntity.class);

    public NugetODataFilterVisitorImpl()
    {
        super();
    }

    @Override
    public Selector<ArtifactEntity> visitFilter(NugetODataFilterParser.FilterContext ctx)
    {
        NugetODataQueryVisitor nugetODataQueryVisitor = new NugetODataQueryVisitor();
        selector.where(nugetODataQueryVisitor.visitFilter(ctx));
        return selector;
    }

}
