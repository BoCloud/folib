//
//
//package com.veadan.folib.scanner.common.base;
//
//
//import com.veadan.folib.scanner.common.msg.TableResultResponse;
//import com.veadan.folib.scanner.common.util.Query;
//import com.github.pagehelper.Page;
//import com.github.pagehelper.PageHelper;
//import org.springframework.beans.factory.annotation.Autowired;
//
//import java.lang.reflect.ParameterizedType;
//import java.util.List;
//import java.util.Map;
//
///**
// * Created by Veadan
// * Date: 17/1/13
// * Time: 15:13
// * Version 1.0.0
// */
//public abstract class BaseBiz<M extends CommonMapper<T>, T> {
//    @Autowired
//    protected M mapper;
//
//    public void setMapper(M mapper) {
//        this.mapper = mapper;
//    }
//
//    public T selectOne(T entity) {
//        return mapper.selectOne(entity);
//    }
//
//
//    public T selectById(Object id) {
//        return mapper.selectByPrimaryKey(id);
//    }
//
//
//    public List<T> selectList(T entity) {
//        return mapper.select(entity);
//    }
//
//
//    public List<T> selectListAll() {
//        return mapper.selectAll();
//    }
//
//
//    public Long selectCount(T entity) {
//        return new Long(mapper.selectCount(entity));
//    }
//
//    public void insertSelective(T entity) {
//        mapper.insertSelective(entity);
//    }
//
//    public void delete(T entity) {
//        mapper.delete(entity);
//    }
//
//
//    public void deleteById(Object id) {
//        mapper.deleteByPrimaryKey(id);
//    }
//
//
//    public void updateById(T entity) {
//        mapper.updateByPrimaryKey(entity);
//    }
//
//
//    public void updateSelectiveById(T entity) {
//        mapper.updateByPrimaryKeySelective(entity);
//
//    }
//
//    public List<T> selectByExample(Object example) {
//        return mapper.selectByExample(example);
//    }
//
//    public int selectCountByExample(Object example) {
//        return mapper.selectCountByExample(example);
//    }
//
//    public TableResultResponse<T> selectByQuery(Query query) {
//        Class<T> clazz = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
//        Example example = new Example(clazz);
//        query2criteria(query, example);
//        Page<Object> result = PageHelper.startPage(query.getPage(), query.getLimit());
//        List<T> list = this.selectByExample(example);
//        return new TableResultResponse<T>(result.getTotal(), list);
//    }
//
//    public void query2criteria(Query query, Example example) {
//        if (query.entrySet().size() > 0) {
//            Query query1 =query;
//            query1.remove("page");
//            query1.remove("limit");
//            Example.Criteria criteria = example.createCriteria();
//            for (Map.Entry<String, Object> entry : query1.entrySet()) {
//                criteria.andLike(entry.getKey(), "%" + entry.getValue().toString() + "%");
//            }
//        }
//    }
//
//}
