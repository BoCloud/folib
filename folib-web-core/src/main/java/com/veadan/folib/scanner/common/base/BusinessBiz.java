

package com.veadan.folib.scanner.common.base;



/**
 * 基础业务类
 * @author Veadan
 * @version 2018/1/13.
 */
public abstract class BusinessBiz<M extends CommonMapper<T>, T>  extends BaseBiz<M, T>  {
    @Override
    public void insertSelective(T entity) {

        super.insertSelective(entity);
    }

    @Override
    public void updateById(T entity) {
        super.updateById(entity);
    }

    @Override
    public void updateSelectiveById(T entity) {
        super.updateSelectiveById(entity);
    }
}
