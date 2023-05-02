# Introduction

This document describes a set of standards for PureScript code under this
project. It also explains our reasoning for these choices, and acts as a living
document of our practices for current and future contributors to the project. We
intend for this document to evolve as our needs change, as well as act as a
single point of truth for standards.

# Motivation

The desired outcomes from the standards specified in this document are as
follows.

## Increased consistency

Inconsistency is worse than _any_ standard, as it requires us to track a large
amount of case-specific information. Software development is already a difficult
task, due to the inherent complexities of the problems we seek to solve, as well
as the inherent complexities foisted on us by _decades_ of bad historical
choices we have no control over. For newcomers to a project and old hands alike,
increased inconsistency translates to developmental friction, resulting in
wasted time, added frustration, and ultimately, worse outcomes for the code in
question.

To avoid putting ourselves into this boat, both now and in the future, we must
aim to be _automatically_ consistent. Similar things should look similar;
different things should look different; as much as possible, we must pick some
rules _and stick to them_; and this has to be clear, explicit, and
well-motivated. This will ultimately benefit us, both in the short and long
term. The standards described here, as well as this document, are written with
this foremost in mind.

## Limited non-local information

There is a limited amount of space in a developer's skull; we all have bad days,
and we forget things or make decisions that, perhaps, may not be ideal at the
time. Therefore, limiting cognitive load is good for us, as it reduces the
amount of trouble we can cause due to said skull limitations. One of the worst
contributors to cognitive load (after inconsistency) is _non-local information_
- the requirement to have some understanding beyond the scope fo the current
unit of work. That unit of work could be a data type, a module, or even a whole
project; in all cases the more non-local information we have to hold in our
minds, the less space we have left for actually doing the task at hand, and the
more mistakes we will make as a consequence.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided; as much locality as possible must be
present everywhere; needless duplication of effort or result must be avoided.
Thus, our work must be broken down into discrete, minimal, logical units, which
can be analyzed, worked on, reviewed and tested in as much isolation as
possible. This also applies to our external dependencies.

Thus, many of the decisions described here are designed to limit the amount of
non-local information needed at all levels of the codebase. Additionally, we
avoid doing things 'just because we can' that would be different for others to
follow, regardless of skill level.

## Minimized legacy impact

Every language, framework, library, or indeed any code at all, contains legacy
decisions. These decisions are frequently outdated, problematic, or outright
wrong. Furthermore, PureScript, like many languages, contains embedded issues or
choices that we must live with, as they cannot be fixed by us, either easily, or
at all. We can't avoid these problems altogether, but we _can_ minimize their
impact on our current work.

Thus, we codify good practices as seen by us, _today_. We also try to avoid
obvious 'sharp edges' by proscribing them away in a principled, consistent and
justifiable manner.

## Drudgery automated away

As developers, we should use our tools to make ourselves as productive as
possible. There's no reason for us to do a task if a machine can do it for us,
especially when this task is something boring or repetitive. We love PureScript
as a language not least of all for its capability to abstract, to describe, and
to make fun what other languages make dull or impossible: our work should do the
same.

Many of the tool-related requirements here are driven by a desire to overcome
boring and repetitive tasks that don't need human intervention. By removing the
need to think about such things, we can focus on those things which _do_ need a
human; thus, we get more done, quicker.

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119].

# Tools

## Compiler warning settings

The following warnings MUST be enabled for all builds fo any project or any
project component:
