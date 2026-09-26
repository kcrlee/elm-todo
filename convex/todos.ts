import { v } from "convex/values";
import { query, mutation } from "./_generated/server";

// The payload shape Elm's todoDecoder expects.
const vTodoDoc = v.object({
  _id: v.id("todos"),
  _creationTime: v.number(),
  title: v.string(),
  description: v.string(),
  isCompleted: v.boolean(),
});

export const list = query({
  args: {},
  returns: v.array(vTodoDoc),
  handler: async (ctx) => {
    return await ctx.db.query("todos").order("desc").take(100);
  },
});

export const create = mutation({
  args: {
    title: v.string(),
    description: v.string(),
  },
  returns: v.null(),
  handler: async (ctx, { title, description }) => {
    await ctx.db.insert("todos", {
      title,
      description,
      isCompleted: false,
    });
    return null;
  },
});

export const markCompleted = mutation({
  args: {
    todoId: v.id("todos"),
    isCompleted: v.boolean(),
  },
  returns: v.null(),
  handler: async (ctx, { todoId, isCompleted }) => {
    await ctx.db.patch("todos", todoId, {
      isCompleted,
    });
    return null;
  },
});
