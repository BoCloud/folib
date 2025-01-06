// const scrollable = {
//     bind(el) {
//       // 初始化滚动条样式类
//       el.classList.add('scrollable');
  
//       // 滚动事件逻辑
//       const onScroll = () => {
//         el.classList.add('scrolling');
  
//         // 滚动停止后 500ms 隐藏滚动条
//         clearTimeout(el._scrollTimeout);
//         el._scrollTimeout = setTimeout(() => {
//           el.classList.remove('scrolling');
//         }, 500);
//       };
  
//       // 绑定滚动事件
//       el._onScroll = onScroll; // 保存事件函数引用，便于解绑
//       el.addEventListener('scroll', onScroll);
//     },
//     unbind(el) {
//       // 移除滚动事件监听
//       el.removeEventListener('scroll', el._onScroll);
//       delete el._onScroll;
//     },
//   };
  
//   export default scrollable;
  