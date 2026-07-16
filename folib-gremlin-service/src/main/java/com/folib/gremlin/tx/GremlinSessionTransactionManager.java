/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.gremlin.tx;

import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.SessionFactory;
import org.neo4j.ogm.transaction.Transaction;
import org.springframework.core.InfrastructureProxy;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;
import org.springframework.transaction.support.TransactionSynchronizationManager;

public class GremlinSessionTransactionManager extends AbstractPlatformTransactionManager {

    private final SessionFactory sessionFactory;

    public GremlinSessionTransactionManager(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    public SessionFactory getSessionFactory() {
        return sessionFactory;
    }

    private Object getResourceKey() {
        if (sessionFactory instanceof InfrastructureProxy) {
            return ((InfrastructureProxy) sessionFactory).getWrappedObject();
        }
        return sessionFactory;
    }

    @Override
    protected Object doGetTransaction() {
        GremlinTransactionObject txObject = new GremlinTransactionObject();
        SessionHolder holder = (SessionHolder) TransactionSynchronizationManager.getResource(getResourceKey());
        if (holder != null) {
            txObject.setSessionHolder(holder);
        }
        return txObject;
    }

    @Override
    protected void doBegin(Object transaction, TransactionDefinition definition) {
        GremlinTransactionObject txObject = (GremlinTransactionObject) transaction;
        Session session = sessionFactory.openSession();
        session.beginTransaction();
        SessionHolder holder = new SessionHolder(session);
        txObject.setSessionHolder(holder);
        TransactionSynchronizationManager.bindResource(getResourceKey(), holder);
    }

    @Override
    protected void doCommit(DefaultTransactionStatus status) {
        GremlinTransactionObject txObject = (GremlinTransactionObject) status.getTransaction();
        Session session = txObject.getSessionHolder().getSession();
        Transaction tx = session.getTransaction();
        if (tx != null) {
            tx.commit();
            tx.close();
        }
    }

    @Override
    protected void doRollback(DefaultTransactionStatus status) {
        GremlinTransactionObject txObject = (GremlinTransactionObject) status.getTransaction();
        Session session = txObject.getSessionHolder().getSession();
        Transaction tx = session.getTransaction();
        if (tx != null) {
            tx.rollback();
            tx.close();
        }
    }

    @Override
    protected void doCleanupAfterCompletion(Object transaction) {
        GremlinTransactionObject txObject = (GremlinTransactionObject) transaction;
        TransactionSynchronizationManager.unbindResourceIfPossible(getResourceKey());
        txObject.getSessionHolder().getSession().clear();
    }

    @Override
    protected boolean isExistingTransaction(Object transaction) {
        GremlinTransactionObject txObject = (GremlinTransactionObject) transaction;
        return txObject.getSessionHolder() != null
                && txObject.getSessionHolder().getSession().getTransaction() != null;
    }

    public static class SessionHolder {
        private final Session session;

        public SessionHolder(Session session) {
            this.session = session;
        }

        public Session getSession() {
            return session;
        }
    }

    private static class GremlinTransactionObject {
        private SessionHolder sessionHolder;

        public SessionHolder getSessionHolder() {
            return sessionHolder;
        }

        public void setSessionHolder(SessionHolder sessionHolder) {
            this.sessionHolder = sessionHolder;
        }
    }
}
