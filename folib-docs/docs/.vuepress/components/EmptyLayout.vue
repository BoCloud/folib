/**
功能：首页
作者：张佳宁
日期：
**/
<template>
    <div class="custom-home">
        <div class="home-navbar">
            <div class="nav-left">
                <img src="../public/logo.png" alt="" width="30">
                <span class="top-title">{{ title }}</span>
                <span style="color:#9CA2A6;">|</span>
                <span class="item-menu">
                    <a style="color: #3f424a;" href="/help/docs/base/artifact-introduction.html">帮助中心</a>
                </span>
            </div>
            <div class="nav-right">
                <a>登录</a>
                <span class="btn">立即注册</span>
            </div>
        </div>
        <div class="content-box">
            <div class="desc">{{ description }}</div>
            <div class="hello-text">{{ hello }}</div>
            <div class="search-container">
                <img src="./images/ai.png" alt="" class="img-ai" width="177">
                <SearchBox ref="searchBox" />
                <span class="searchBtn" @click="searchFn">搜索</span>
            </div>
        </div>
        <div class="menu-list">
            <div class="menu-box" v-for="item, index in menuList" :key="index">
                <div class="menu-title">
                    <img :src="item.imageUrl" alt="" width="28" height="28">
                    <span class="menu-title_text">{{ item.title }}</span>
                </div>
                <div class="menu-title_text_child" v-for="itm, i in item.menuListChild" :key="i">
                    <a :href="itm.url">{{ itm.title }}</a>
                </div>
                <div class="view-all">
                    <a :href="item.viewAllUrl">查看全部 ></a>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
import { description } from '../config';
import searchResult from './self-components/search-result.vue';
import removeMarkdown from "remove-markdown";
import SearchBox from '@vuepress/plugin-search/SearchBox.vue';

export default {
    components: {
        searchResult,SearchBox
    },
    name: "EmptyLayout",
    data() {
        return {
            title: 'Fo Library',
            description: 'FOLIB，通用企业级制品管理系统～🎉',
            hello: 'Hi，请问有什么可以帮您？',
            searchText: '',
            menuList: [
                {
                    title: '基础知识',
                    imageUrl: require('./images/icon1@2x.png'),
                    menuListChild: [
                        {
                            title: '基础知识：什么是制品',
                            url: '/help/docs/base/artifact-introduction.html'
                        },
                        {
                            title: '基础知识：什么是制品库',
                            url: '/help/docs/base/library-introduction.html'
                        },
                        {
                            title: '基础知识：常用名词定义',
                            url: '/help/docs/base/warehouse-type-introduction.html'
                        },
                    ],
                    viewAllUrl: '/help/docs/base/artifact-introduction.html'
                },
                {
                    title: '制品管理',
                    imageUrl: require('./images/icon2@2x.png'),
                    menuListChild: [
                        {
                            title: '制品管理：存储空间管理',
                            url: '/help/docs/storage-space/storage-space-outline.html'
                        },
                        {
                            title: '制品管理：制品仓库管理',
                            url: '/help/docs/warehouse/warehouse-outline.html'
                        },
                        {
                            title: '制品管理：制品文件管理',
                            url: '/help/docs/warefile/warefile-outline.html'
                        }
                    ],
                    viewAllUrl: '/help/docs/storage-space/storage-space-outline.html'
                },
                {
                    title: '部署、监控与维护',
                    imageUrl: require('./images/icon3@2x.png'),
                    menuListChild: [
                        {
                            title: '安装维护：虚拟机安装',
                            url: '/help/docs/deploy/vm-deploy.html'
                        },
                        {
                            title: '安装维护：Docker容器安装',
                            url: '/help/docs/deploy/docker-deploy.html'
                        },
                        {
                            title: '安装维护：集群在线维护',
                            url: '/help/docs/deploy/operation.html'
                        },
                    ],
                    viewAllUrl: '/help/docs/deploy/vm-deploy.html'
                }
            ],
            show: false,
            results: []
        }
    },
    mounted() {
        function validSubstringCount(word1, word2) {
            let tarArr = word2.split("")
            let oriArr = word1.split("")
            let obj = {}
            let arr = []
            tarArr.forEach(ele => {
                obj[ele] = 0
            })
            oriArr.forEach(ele => {
                for (let key in obj) {
                    if (ele == key) {
                        obj[key]++
                    }
                }
            })
            for (let key in obj) {
                arr.push(obj[key])
            }
            const minValue = Math.min(...arr)

            return minValue.toString(2)
        };
        console.log(validSubstringCount('bcca','abc'))
    },
    methods: {
        searchFn(){
            if(!this.$refs.searchBox.query){
                this.$notify({
                    title: '警告',
                    message: '请输入关键字搜索...',
                    type: 'warning'
                });
            }
        },
        getInput() {
            this.show = true
            if (!this.searchText) {
                this.show = false
            }
            this.performSearch()
        },
        blur() {
            if (!this.searchText) {
                this.show = false
            }
        },
        focus() {
            if (this.searchText) {
                this.show = true
                this.performSearch()
            }
        },
        performSearch() {
            const query = this.searchText
            if (!query) {
                this.results = []
                return
            }
            console.log(this.$site.pages)
            this.results = this.$site.pages
                .filter((page) => {
                    if (page.relativePath != 'README.md') {
                        // const content = page.contentStripped || ''
                        return (
                            page.title.includes(query)
                            // || content.includes(query)
                        )
                    }
                })
                .map((page) => ({
                    title: page.title,
                    path: page.path,
                    // summary: this.extractSummary(page, query),
                }))
            console.log(this.results)
        },
        extractSummary(page, query) {
            const content = page.contentStripped || ""
            let cleanContent = removeMarkdown(content)
            cleanContent = cleanContent.replace(/\r?\n|\r/g, ' ')

            cleanContent = cleanContent.replace(/\n{2,}/g, '\n')

            cleanContent = cleanContent.replace(/title:*\n/g, '')

            const index = cleanContent.indexOf(query);

            if (index === -1) return "";

            // 提取目标内容前后四个字
            const start = Math.max(index - 4, 0); // 确保不会小于0
            const end = Math.min(index + query.length + 4, cleanContent.length); // 确保不会超出内容长度

            const summary = cleanContent.substring(start, end).trim();

            return summary.length < 9 ? summary : summary.slice(0, 10) + "...";
        }
    }
};
</script>

<style lang="scss" scoped>
/* 自定义你的样式 */
.custom-home {
    height: 100vh;
    overflow-y: auto;
    overflow-x: hidden;
    width: 100%;
    text-align: center;
    background-image: url('./images/bg@2x.png');
    background-repeat: no-repeat;
    background-size: cover;

    .home-navbar {
        color: #3F424A;
        display: flex;
        height: 72px;
        width: 100%;
        justify-content: space-around;
        align-items: center;
        // border-bottom: 1px solid #1890ff;
        font-size: 14px;

        .nav-left {
            display: flex;
            align-items: center;

            .top-title {
                font-weight: 600;
                font-size: 18px;
                margin: auto 20px;
            }

            .item-menu {
                margin: auto 20px;
            }
        }

        .nav-right {
            a {
                cursor: pointer;
            }

            .btn {
                display: inline-block;
                margin-left: 30px;
                width: 117px;
                height: 35px;
                border-radius: 6px;
                border: 1px solid #1890FF;
                line-height: 35px;
                color: #1890FF;
                cursor: pointer;

                &:hover {
                    background-color: rgb(238, 244, 252);
                }
            }
        }
    }

    .content-box {

        margin: 76px auto;
        height: 220px;

        .desc {
            font-size: 20px;
            color: #7A7B7D;
        }

        .hello-text {
            margin: 25px auto;
            font-size: 39px;
            color: #0B2A42;
            font-weight: 500;
        }

        .search-container {
            position: relative;
            display: flex;
            width: 720px;
            height: 56px;
            background: #FFFFFF;
            box-shadow: 0px 8px 14px 0px rgba(214, 217, 226, 0.5);
            border-radius: 28px;
            margin: 70px auto;

            .search-input {
                display: flex;
                border: none;
                height: 56px;
                font-size: 14px;
                width: 520px;
            }

            .img-ai{
                position: absolute;
                top: -145px;
                left: -40px;
            }

            input {
                outline: none;
                background: transparent;
                border: none;
                outline: medium;
                padding-left: 0 !important;
            }

            .search-icon {
                height: 56px;
                width: 70px;
                // padding-left: 20px;
                text-align: center;
                line-height: 56px;
            }

            .searchBtn {
                position: absolute;
                display: inline-block;
                width: 98px;
                height: 46px;
                background: #1890FF;
                border-radius: 23px;
                color: #fff;
                line-height: 46px;
                top: 5px;
                right: 5px;
                cursor: pointer;

                &:hover {
                    background: #178efd;
                }
            }

        }
    }

    .menu-list {
        display: flex;
        margin: 0 auto;
        justify-content: center;

        .menu-box {
            position: relative;
            padding: 30px 34px;
            text-align: left;
            box-sizing: border-box;
            width: 386px;
            height: 260px;
            background: linear-gradient(rgba(255, 255, 255, 0.39) 0%, #FFFFFF 100%);
            box-shadow: 0px 19px 68px -32px rgba(29, 75, 165, 0.16);
            border-radius: 7px;
            border: 1px solid;
            border-image: linear-gradient(180deg, rgba(255, 255, 255, 1), rgba(255, 255, 255, 0.16)) 1 1;
            backdrop-filter: blur(29px);
            margin-left: 20px;

            .menu-title {
                display: flex;
                height: 28px;
                align-items: center;
                margin-bottom: 20px;

                .menu-title_text {
                    font-size: 16px;
                    font-weight: 600;
                    margin-left: 14px;
                }
            }

            .menu-title_text_child {
                cursor: pointer;
                margin-bottom: 18px;
                height: 14px;
                font-family: PingFangSC, PingFang SC;
                font-weight: 400;
                font-size: 14px;
                color: #5D7284;
                line-height: 14px;
                text-align: left;
                font-style: normal;
                border-left: 1px solid #66717a;
                padding-left: 10px;
                width: 80%;
                overflow: hidden;
                text-overflow: ellipsis;
                white-space: nowrap;

                a {
                    color: #5D7284;
                }
            }

            .view-all {
                position: absolute;
                font-size: 14px;
                bottom: 20px;
                right: 30px;
            }
        }
    }
}
</style>
<style lang="scss">
.custom-home{

    .search-box{
        width: calc(100% - 168px);
        input {
            outline: none;
            // background: transparent;
            border: none;
            outline: medium;
            padding-left: 60px !important;
            padding-right: 100px !important;
            width: 100%;
            height: 56px;
        }
    }

    .search-container .search-box input{
        background-position: 22px;
    }
    .search-container{
        .suggestions{
            padding: 20px;
            box-sizing: border-box;
            position: absolute;
            text-align: left;
            max-height: 300px;
            overflow-y:auto;
            width: 720px;
            top: 52px;
            border:none;
            border-radius: 8px;
            background: rgba(255, 255, 255, 0.5);
            z-index: 999;
            backdrop-filter: blur(20px);
            box-shadow: 0px 8px 14px 0px rgba(214, 217, 226, 0.5);

            .suggestion{
                &:hover{
                    background: #fff !important;
                }
            }
        }
    }
}
</style>
