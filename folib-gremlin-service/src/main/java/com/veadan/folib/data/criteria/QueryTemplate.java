package com.veadan.folib.data.criteria;

import com.veadan.folib.data.domain.DomainObject;

/**
 * You can perform concrete queries under concrete DB implementations with
 * implementations of {@link QueryTemplate}.
 * 
 * @author veadan
 *
 */
public interface QueryTemplate<R, T extends DomainObject>
{

    R select(Selector<T> s);

}
